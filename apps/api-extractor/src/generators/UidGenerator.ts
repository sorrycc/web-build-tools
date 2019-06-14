// Copyright (c) Microsoft Corporation. All rights reserved. Licensed under the MIT license.
// See LICENSE in the project root for license information.

// tslint:disable:no-bitwise

import * as ts from 'typescript';
import { PackageJsonLookup, INodePackageJson } from '@microsoft/node-core-library';
import { TypeScriptInternals, IMappedType } from '../analyzer/TypeScriptInternals';
import { TypeScriptHelpers } from '../analyzer/TypeScriptHelpers';

interface ISymbolInfo {
  readonly symbol: ts.Symbol;
  readonly baseUid: string;
  readonly meanings: ReadonlyMap<ts.SymbolFlags, IMeaningInfo>;
}

interface IMeaningInfo {
  readonly arity: string;
  readonly disambiguation: string;
  readonly uid: string;
}

/**
 * An object that can generate UIDs for Declarations, Symbols, and Types in a program.
 *
 * @public
 */
export class UidGenerator {
  private _packageJsonLookup: PackageJsonLookup;
  private _program: ts.Program;
  private _typeChecker: ts.TypeChecker;
  private _declarationCache: WeakMap<ts.Declaration, string>;
  private _symbolCache: WeakMap<ts.Symbol, ISymbolInfo>;
  private _typeCache: WeakMap<ts.Type, string>;
  private _inferTypeParameters: ts.TypeParameter[] | undefined;
  private _workingPackageName: string;

  constructor(packageJsonLookup: PackageJsonLookup, program: ts.Program, typeChecker: ts.TypeChecker,
    workingPackageName: string) {
    this._packageJsonLookup = packageJsonLookup;
    this._program = program;
    this._typeChecker = typeChecker;
    this._declarationCache = new WeakMap<ts.Declaration, string>();
    this._symbolCache = new WeakMap<ts.Symbol, ISymbolInfo>();
    this._typeCache = new WeakMap<ts.Type, string>();
    this._workingPackageName = workingPackageName;
  }

  /**
   * Get the UID for a declaration.
   */
  public getUidOfDeclaration(node: ts.Declaration): string {
    let uid: string | undefined = this._declarationCache.get(node);
    if (uid === undefined) {
      const symbol: ts.Symbol | undefined = TypeScriptInternals.tryGetSymbolForDeclaration(node, this._typeChecker);
      if (symbol) {
        uid = this.getUidOfSymbol(symbol, getMeaningOfDeclaration(node));
        if (ts.isFunctionLike(node)) {
          const signature: ts.Signature | undefined = this._typeChecker.getSignatureFromDeclaration(node);
          if (signature) {
            uid += this.signatureToUidComponent(signature);
          }
        }
      } else {
        uid = '';
      }
      this._declarationCache.set(node, uid);
    }
    return uid;
  }

  /**
   * Get the UID for a symbol given its meaning.
   */
  public getUidOfSymbol(symbol: ts.Symbol, meaning: ts.SymbolFlags): string {
    const exportSymbol: ts.Symbol = this._typeChecker.getExportSymbolOfSymbol(symbol);
    if (!(symbol.flags & meaning) && !(exportSymbol.flags & meaning)) {
      return '';
    }
    const symbolInfo: ISymbolInfo = this.getSymbolInfoOfSymbol(exportSymbol);
    return this.symbolInfoToUid(symbolInfo, meaning);
  }

  /**
   * Get the UID for a Type.
   */
  public getUidOfType(type: ts.Type): string {
    let uid: string | undefined = this._typeCache.get(type);
    if (uid === undefined) {
      uid = this.typeToUid(type);
      this._typeCache.set(type, uid);
    }
    return uid;
  }

  /**
   * Gets the base uid of a symbol and the uids for all of the symbol's meanings.
   */
  private getSymbolInfoOfSymbol(symbol: ts.Symbol): ISymbolInfo {
    let symbolInfo: ISymbolInfo | undefined = this._symbolCache.get(symbol);
    if (symbolInfo === undefined) {
      symbolInfo = this.symbolToSymbolInfo(symbol);
      this._symbolCache.set(symbol, symbolInfo);
    }
    return symbolInfo;
  }

  /**
   * Creates an ISymbolInfo containing the base uid of a symbol and the uids for all of the symbol's meanings.
   */
  private symbolToSymbolInfo(symbol: ts.Symbol): ISymbolInfo {
    // TODO: handle symbols from instantations of generics (preserve type arguments)

    const baseUid: string = this.symbolToBaseUid(symbol);
    const meanings: Map<ts.SymbolFlags, IMeaningInfo> = new Map<ts.SymbolFlags, IMeaningInfo>();
    const seen: Set<string> = new Set<string>();
    for (const meaning of getPrioritizedMeanings(symbol.flags)) {
      let uid: string = baseUid;

      // For types, add the generic arity.
      const arity: string = getArityForMeaningOfSymbol(symbol, meaning);
      uid += arity;

      // If necessary, disambiguate.
      const disambiguation: string = disambiguateUid(uid, meaning, seen);
      uid += disambiguation;

      // Track all of the components so that we can support generic instantiations.
      meanings.set(meaning, { arity, disambiguation, uid });
      seen.add(uid);
    }

    return { symbol, baseUid, meanings };
  }

  /**
   * Generates the base UID of a symbol.
   */
  private symbolToBaseUid(symbol: ts.Symbol): string {
    // Module symbols do not have uids.
    if (isExternalModuleSymbol(symbol)) {
      return '';
    }

    if (symbol.flags & ts.SymbolFlags.TypeParameter) {
      // TODO: local type parameters `n and ``n if this is feasable. This may require location
      // information and may not be feasible.
      return `{${symbol.name}}`;
    }

    const baseUid: string = this.symbolNameToUidComponent(symbol);
    const parent: ts.Symbol | undefined = TypeScriptInternals.getSymbolParent(symbol);
    const parentInfo: ISymbolInfo | undefined = parent && !isExternalModuleSymbol(parent)
      ? this.getSymbolInfoOfSymbol(parent)
      : undefined;

    if (parentInfo) {
      if (isTypeMemberOrNonStaticClassMember(symbol)) {
        return `${parentInfo.baseUid}#${baseUid}`;
      }

      return `${parentInfo.baseUid}.${baseUid}`;
    }

    const sourceFile: ts.SourceFile | undefined =
      symbol.declarations
      && symbol.declarations[0]
      && symbol.declarations[0].getSourceFile();

    if (sourceFile && ts.isExternalModule(sourceFile)) {
      return this.packageComponentOfSymbol(symbol) + baseUid;
    }

    return baseUid;
  }

  /**
   * Gets the UID for a particular meaning of a symbol.
   */
  private symbolInfoToUid(symbolInfo: ISymbolInfo, meaning: ts.SymbolFlags): string {
    // First try to get the meaning directly...
    let meaningInfo: IMeaningInfo | undefined = symbolInfo.meanings.get(symbolInfo.symbol.flags & meaning);
    if (meaningInfo === undefined) {
      // Failing that, scan each key to find a match for the meaning.
      const matchingInfos: IMeaningInfo[] = [];
      for (const [key, value] of symbolInfo.meanings) {
        if (meaning & key) {
          matchingInfos.push(value);
        }
      }

      // If there are no matches, there is no UID.
      if (matchingInfos.length === 0) {
        return '';
      }

      // If there are multiple matching uids, always pick the one with the highest precedence.
      meaningInfo = matchingInfos[0];
    }

    return meaningInfo.uid;
  }

  /**
   * Gets the package component of a Symbol.
   */
  private packageComponentOfSymbol(symbol: ts.Symbol): string {
    const sourceFile: ts.SourceFile = symbol.declarations[0].getSourceFile();
    if (this._program.isSourceFileFromExternalLibrary(sourceFile)) {
      const packageJson: INodePackageJson | undefined = this._packageJsonLookup
        .tryLoadNodePackageJsonFor(sourceFile.fileName);
      if (packageJson && packageJson.name) {
        return packageJson.name + '/';
      } else {
        return '';
      }
    } else {
      return this._workingPackageName + '/';
    }
  }

  /**
   * Gets the UID component for the name of a Symbol.
   */
  private symbolNameToUidComponent(symbol: ts.Symbol): string {
    let localName: string = symbol.name;
    if (TypeScriptHelpers.isWellKnownSymbolName(localName)) {
      // TypeScript binds well-known ECMAScript symbols like 'Symbol.iterator' as '__@iterator'.
      // This converts a string like '__@iterator' into the property name '[Symbol.iterator]'.
      localName = `[Symbol.${localName.slice(3)}]`;
    } else if (TypeScriptHelpers.isUniqueSymbolName(localName)) {
      for (const decl of symbol.declarations || []) {
        const declName: ts.DeclarationName | undefined = ts.getNameOfDeclaration(decl);
        if (declName && ts.isComputedPropertyName(declName)) {
          const lateName: string | undefined = TypeScriptHelpers.tryGetLateBoundName(declName);
          if (lateName !== undefined) {
            localName = lateName;
            break;
          }
        }
      }
    }
    return localName;
  }

  /**
   * Gets the UID component for a signature.
   */
  private signatureToUidComponent(signature: ts.Signature): string {
    let uid: string = '';
    if (signature.typeParameters && signature.typeParameters.length) {
      uid += `\`\`${signature.typeParameters.length}`;
    }
    if (signature.parameters.length) {
      uid += `(${signature.parameters.map(p => this.parameterToUid(p)).join(',')})`;
    }
    return uid;
  }

  /**
   * Gets the UID component for a parameter.
   */
  private parameterToUid(symbol: ts.Symbol): string {
    const type: ts.Type = this._typeChecker.getTypeOfSymbolAtLocation(symbol, symbol.declarations[0]);
    const uid: string = this.getUidOfType(type);
    if (symbol.valueDeclaration && ts.isParameter(symbol.valueDeclaration)) {
      if (symbol.valueDeclaration.dotDotDotToken) {
        return '...' + uid;
      }
      if (symbol.valueDeclaration.questionToken) {
        return '?' + uid;
      }
    }
    return uid;
  }

  /**
   * Creates a UID from a Type.
   */
  private typeToUid(type: ts.Type): string {
    if (type.flags & ts.TypeFlags.Any) {
      return 'any';
    }
    if (type.flags & ts.TypeFlags.Unknown) {
      return 'unknown';
    }
    if (type.flags & ts.TypeFlags.String) {
      return 'string';
    }
    if (type.flags & ts.TypeFlags.Number) {
      return 'number';
    }
    if (type.flags & ts.TypeFlags.BigInt) {
      return 'bigint';
    }
    if (type.flags & ts.TypeFlags.Boolean) {
      return 'boolean';
    }
    if (type.flags & ts.TypeFlags.Void) {
      return 'void';
    }
    if (type.flags & ts.TypeFlags.Undefined) {
      return 'undefined';
    }
    if (type.flags & ts.TypeFlags.Null) {
      return 'null';
    }
    if (type.flags & ts.TypeFlags.Never) {
      return 'never';
    }
    if (type.flags & ts.TypeFlags.ESSymbol) {
      return 'symbol';
    }
    if (type.flags & ts.TypeFlags.NonPrimitive) {
      return 'object';
    }
    if (type.flags & ts.TypeFlags.TypeParameter && TypeScriptInternals.isThisType(type)) {
      return 'this';
    }
    if (type.flags & ts.TypeFlags.EnumLiteral && !(type.flags & ts.TypeFlags.Union)) {
      const parent: ts.Symbol = TypeScriptInternals.getSymbolParent(type.symbol)!;
      if (this._typeChecker.getDeclaredTypeOfSymbol(parent) === type) {
        return this.getUidOfSymbol(parent, ts.SymbolFlags.Enum);
      }
      return this.getUidOfSymbol(type.symbol, ts.SymbolFlags.EnumMember);
    }
    if (type.flags & ts.TypeFlags.EnumLike) {
      return this.getUidOfSymbol(type.symbol, ts.SymbolFlags.EnumMember);
    }
    if (type.flags & ts.TypeFlags.StringLiteral) {
      return JSON.stringify((type as ts.StringLiteralType).value);
    }
    if (type.flags & ts.TypeFlags.NumberLiteral) {
      return JSON.stringify((type as ts.NumberLiteralType).value);
    }
    if (type.flags & ts.TypeFlags.BigIntLiteral) {
      const { negative, base10Value } = (type as ts.BigIntLiteralType).value;
      return `${negative ? '-' : ''}${base10Value}u`;
    }
    if (type.flags & ts.TypeFlags.BooleanLiteral) {
      return this._typeChecker.typeToString(type);
    }
    if (type.flags & ts.TypeFlags.UniqueESSymbol) {
      return `typeof~${this.getUidOfSymbol(type.symbol, ts.SymbolFlags.Value)}`;
    }
    const objectFlags: ts.ObjectFlags = (type as ts.ObjectType).objectFlags;
    if (objectFlags & ts.ObjectFlags.Reference) {
      return this.typeReferenceToUid(type as ts.TypeReference);
    }
    if (type.flags & ts.TypeFlags.TypeParameter || objectFlags & ts.ObjectFlags.ClassOrInterface) {
      if (type.flags & ts.TypeFlags.TypeParameter
        && this._inferTypeParameters
        && this._inferTypeParameters.indexOf(type) !== -1) {
        return `infer~${this.typeParameterToUid(type as ts.TypeParameter)}`;
      }
      let uid: string = type.symbol ? this.getUidOfSymbol(type.symbol, ts.SymbolFlags.Type) : '?';
      if (objectFlags & ts.ObjectFlags.ClassOrInterface) {
        if ((type as ts.GenericType).typeParameters && (type as ts.GenericType).typeParameters!.length) {
          uid += `\`${(type as ts.GenericType).typeParameters!.length}`;
        }
      }
      return uid;
    }
    if (type.aliasSymbol) {
      return this.typeAliasToUid(type.aliasSymbol, type.aliasTypeArguments);
    }
    if (type.flags & ts.TypeFlags.Union) {
      return (type as ts.UnionType).types.map(t => this.getUidOfType(t)).join('|');
    }
    if (type.flags & ts.TypeFlags.Intersection) {
      return (type as ts.IntersectionType).types.map(t => this.getUidOfType(t)).join('&');
    }
    if (objectFlags & (ts.ObjectFlags.Anonymous | ts.ObjectFlags.Mapped)) {
      return this.anonymousTypeToUid(type as ts.ObjectType);
    }
    if (type.flags & ts.TypeFlags.Index) {
      return this.indexTypeToUid(type as ts.IndexType);
    }
    if (type.flags & ts.TypeFlags.IndexedAccess) {
      return this.indexedAccessToUid(type as ts.IndexedAccessType);
    }
    if (type.flags & ts.TypeFlags.Conditional) {
      return this.conditionalTypeToUid(type as ts.ConditionalType);
    }
    return escapeUidFragment(this._typeChecker.typeToString(type));
  }

  private typeReferenceToUid(type: ts.TypeReference): string {
    let uid: string = this.getUidOfSymbol(type.symbol, ts.SymbolFlags.Type);
    if (type.typeArguments && type.typeArguments.length) {
      uid += `{${type.typeArguments.map(t => this.getUidOfType(t)).join(',')}}`;
    }
    return uid;
  }

  private typeParameterToUid(type: ts.TypeParameter): string {
    return this.getUidOfSymbol(type.symbol, ts.SymbolFlags.TypeParameter);
  }

  private typeAliasToUid(aliasSymbol: ts.Symbol, typeArguments: ReadonlyArray<ts.Type> | undefined): string {
    let uid: string = this.getUidOfSymbol(aliasSymbol, ts.SymbolFlags.TypeAlias);
    if (typeArguments && typeArguments.length) {
      uid += `${typeArguments.map(t => this.getUidOfType(t)).join(',')}`;
    }
    return uid;
  }

  private anonymousTypeToUid(type: ts.ObjectType): string {
    const mappedType: IMappedType | undefined = TypeScriptInternals.getMappedType(type);
    if (mappedType) {
      const typeParameter: string = this.mappedTypeParameterToUid(mappedType);
      const constraint: string = this.mappedTypeConstraintToUid(mappedType);
      const template: string = this.mappedTypeTemplateToUid(mappedType);
      const readonly: string = this.mappedTypeReadonlyTokenToUidComponent(mappedType);
      const question: string = this.mappedTypeQuestionTokenToUidComponent(mappedType);
      return `{{${readonly}[${typeParameter}~in~${constraint}]${question}:${template}}}`;
    }
    const properties: ts.Symbol[] = this._typeChecker.getPropertiesOfType(type);
    const callSignatures: ReadonlyArray<ts.Signature> =
      this._typeChecker.getSignaturesOfType(type, ts.SignatureKind.Call);
    const constructSignatures: ReadonlyArray<ts.Signature> =
      this._typeChecker.getSignaturesOfType(type, ts.SignatureKind.Construct);
    const stringIndex: ts.IndexInfo | undefined = this._typeChecker.getIndexInfoOfType(type, ts.IndexKind.String);
    const numberIndex: ts.IndexInfo | undefined = this._typeChecker.getIndexInfoOfType(type, ts.IndexKind.Number);
    if (!properties.length && !stringIndex && !numberIndex) {
      if (!callSignatures.length && !constructSignatures.length) {
        return '{{}}';
      }
      if (callSignatures.length === 1 && !constructSignatures.length) {
        const signatureComponent: string = this.signatureToUidComponent(callSignatures[0]) || '()';
        const returnType: ts.Type = this._typeChecker.getReturnTypeOfSignature(callSignatures[0]);
        const returnTypeComponent: string = this.getUidOfType(returnType);
        return `${signatureComponent}~${returnTypeComponent}`;
      }
      if (constructSignatures.length === 1 && !callSignatures.length) {
        const signatureComponent: string = this.signatureToUidComponent(constructSignatures[0]) || '()';
        const returnType: ts.Type = this._typeChecker.getReturnTypeOfSignature(constructSignatures[0]);
        const returnTypeComponent: string = this.getUidOfType(returnType);
        return `new${signatureComponent}~${returnTypeComponent}`;
      }
    }
    // TODO: signatures, indexers
    return `{{${properties.map(p => this.propertyToUidComponent(p)).join(',')}}}`;
  }

  private mappedTypeReadonlyTokenToUidComponent(type: IMappedType): string {
    const token: ts.MappedTypeNode['readonlyToken'] = type.declaration.readonlyToken;
    return !token ? '' :
      token.kind === ts.SyntaxKind.PlusToken ? '+readonly~' :
      token.kind === ts.SyntaxKind.MinusToken ? '-readonly~' :
      'readonly~';
  }

  private mappedTypeQuestionTokenToUidComponent(type: IMappedType): string {
    const token: ts.MappedTypeNode['questionToken'] = type.declaration.questionToken;
    return !token ? '' :
      token.kind === ts.SyntaxKind.PlusToken ? '+?' :
      token.kind === ts.SyntaxKind.MinusToken ? '-?' :
      '?';
  }

  private mappedTypeParameterToUid(type: IMappedType): string {
    return type.typeParameter ? this.getUidOfSymbol(type.typeParameter.symbol, ts.SymbolFlags.TypeParameter) :
      this.getUidOfDeclaration(type.declaration.typeParameter);
  }

  private mappedTypeConstraintToUid(type: IMappedType): string {
    return type.constraintType ? this.getUidOfType(type.constraintType) :
      this.typeParameterConstraintToUidComponent(type.declaration.typeParameter);
  }

  private mappedTypeTemplateToUid(type: IMappedType): string {
    if (type.templateType) {
      return this.getUidOfType(type.templateType);
    }
    if (type.declaration.type) {
      return this.getUidOfType(this._typeChecker.getTypeFromTypeNode(type.declaration.type));
    }
    return '?';
  }

  private typeParameterConstraintToUidComponent(node: ts.TypeParameterDeclaration): string {
    const constraint: ts.TypeNode | undefined = ts.getEffectiveConstraintOfTypeParameter(node);
    if (constraint) {
      const type: ts.Type = this._typeChecker.getTypeFromTypeNode(constraint);
      return this.getUidOfType(type);
    }
    return '';
  }

  /**
   * Converts a property Symbol into a UID component.
   *
   * For example: `x?: T` -> `x?:T`
   */
  private propertyToUidComponent(property: ts.Symbol): string {
    const name: string = this.symbolNameToUidComponent(property);
    const question: string = property.flags & ts.SymbolFlags.Optional ? '?' : '';
    const type: ts.Type = this._typeChecker.getTypeOfSymbolAtLocation(property, property.declarations[0]);
    const typeUid: string = this.getUidOfType(type);
    return `${name}${question}:${typeUid}`;
  }

  /**
   * Converts an IndexType into a UID.
   *
   * For example: `keyof T` becomes `keyof~T`
   */
  private indexTypeToUid(type: ts.IndexType): string {
    return `keyof~${this.getUidOfType(type.type)}`;
  }

  /**
   * Converts an IndexedAccessType into a UID.
   *
   * For example: `T[K]` -> `T[K]` (with transformations on `T` and `K`).
   */
  private indexedAccessToUid(type: ts.IndexedAccessType): string {
    return `${this.getUidOfType(type.objectType)}[${this.getUidOfType(type.indexType)}]`;
  }

  /**
   * Converts a ConditionalType into a UID.
   *
   * For example: `S extends T ? U : V` -> `S~extends~T?U:V`.
   */
  private conditionalTypeToUid(type: ts.ConditionalType): string {
    const checkUid: string = this.getUidOfType(type.checkType);
    const savedInferTypeParameters: ts.TypeParameter[] | undefined = this._inferTypeParameters;
    this._inferTypeParameters = type.root.inferTypeParameters;
    const extendsUid: string = this.getUidOfType(type.extendsType);
    this._inferTypeParameters = savedInferTypeParameters;
    const trueUid: string = this.getUidOfType(type.trueType);
    const falseUid: string = this.getUidOfType(type.falseType);
    return `${checkUid}~extends~${extendsUid}?${trueUid}:${falseUid}`;
  }
}

function isExternalModuleSymbol(symbol: ts.Symbol): boolean {
  return !!(symbol.flags & ts.SymbolFlags.ValueModule)
    && symbol.valueDeclaration !== undefined
    && ts.isSourceFile(symbol.valueDeclaration);
}

function isTypeMemberOrNonStaticClassMember(symbol: ts.Symbol): boolean {
  if (symbol.valueDeclaration) {
    if (ts.isClassLike(symbol.valueDeclaration.parent)) {
      return ts.isClassElement(symbol.valueDeclaration)
        && !(ts.getCombinedModifierFlags(symbol.valueDeclaration) & ts.ModifierFlags.Static);
    }
    if (ts.isInterfaceDeclaration(symbol.valueDeclaration.parent)) {
      return ts.isTypeElement(symbol.valueDeclaration);
    }
  }
  return false;
}

function getArityOfDeclaration(node: ts.Declaration): number {
  if (ts.isClassDeclaration(node)
    || ts.isClassExpression(node)
    || ts.isInterfaceDeclaration(node)
    || ts.isTypeAliasDeclaration(node)) {
    if (node.typeParameters) {
      return node.typeParameters.length;
    }
  }
  return 0;
}

/**
 * Gets a disambiguation qualifier for a UID if one is needed.
 */
function disambiguateUid(baseUid: string, meaning: ts.SymbolFlags, seenUids: Set<string>): string {
  let meaningDisambiguation: string = '';
  if (seenUids.has(baseUid)) {
    meaningDisambiguation = meaningToUidComponent(meaning);
  }

  let attempt: number = 0;
  let disambiguation: string;
  do {
    disambiguation = attempt === 0 ? meaningDisambiguation : `${meaningDisambiguation}@${attempt}`;
    attempt++;
  } while (seenUids.has(baseUid + disambiguation));

  return disambiguation;
}

function meaningToUidComponent(meaning: ts.SymbolFlags): string {
  if (meaning & ts.SymbolFlags.Class) {
    return ':class';
  }
  if (meaning & ts.SymbolFlags.Enum) {
    return ':enum';
  }
  if (meaning & ts.SymbolFlags.Interface) {
    return ':interface';
  }
  if (meaning & ts.SymbolFlags.TypeAlias) {
    return ':typealias';
  }
  if (meaning & ts.SymbolFlags.TypeParameter) {
    return ':typeparameter';
  }
  if (meaning & ts.SymbolFlags.Function) {
    return ':function';
  }
  if (meaning & ts.SymbolFlags.Variable) {
    return ':variable';
  }
  if (meaning & ts.SymbolFlags.Module) {
    return ':namespace';
  }
  if (meaning & ts.SymbolFlags.ClassMember) {
    return ':member';
  }
  if (meaning & ts.SymbolFlags.Constructor) {
    return ':constructor';
  }
  if (meaning & ts.SymbolFlags.EnumMember) {
    return ':enummember';
  }
  return `:${meaning}`; // unknown meaning. Encode it as the number.
}

function getMeaningOfDeclaration(node: ts.Declaration): ts.SymbolFlags {
  const meaning: ts.SymbolFlags =
    ts.isClassDeclaration(node) ? ts.SymbolFlags.Class :
    ts.isClassExpression(node) ? ts.SymbolFlags.Class :
    ts.isEnumDeclaration(node) ? ts.SymbolFlags.Enum :
    ts.isInterfaceDeclaration(node) ? ts.SymbolFlags.Interface :
    ts.isTypeAliasDeclaration(node) ? ts.SymbolFlags.TypeAlias :
    ts.isTypeParameterDeclaration(node) ? ts.SymbolFlags.TypeParameter :
    ts.isFunctionDeclaration(node) ? ts.SymbolFlags.Function :
    ts.isFunctionExpression(node) ? ts.SymbolFlags.Function :
    ts.isVariableDeclaration(node) ? ts.SymbolFlags.Variable :
    ts.isParameter(node) ? ts.SymbolFlags.Variable :
    ts.isBindingElement(node) ? ts.SymbolFlags.Variable :
    ts.isModuleDeclaration(node) ? ts.SymbolFlags.Module :
    ts.isSourceFile(node) ? ts.SymbolFlags.Module :
    ts.isConstructorDeclaration(node) ? ts.SymbolFlags.Constructor :
    ts.isClassElement(node) ? ts.SymbolFlags.ClassMember :
    ts.isTypeElement(node) ? ts.SymbolFlags.ClassMember :
    ts.isEnumMember(node) ? ts.SymbolFlags.EnumMember :
    ~ts.SymbolFlags.None;
  return meaning;
}

function getPrioritizedMeanings(meaning: ts.SymbolFlags): ReadonlyArray<ts.SymbolFlags> {
  // NOTE: These are added in order of precedence, with earlier elements having a higher
  // precedence than later elements.
  const meanings: ts.SymbolFlags[] = [];
  if (meaning & ts.SymbolFlags.Class) {
    meanings.push(ts.SymbolFlags.Class);
  }
  if (meaning & ts.SymbolFlags.Enum) {
    meanings.push(meaning & ts.SymbolFlags.Enum);
  }
  if (meaning & ts.SymbolFlags.Interface) {
    meanings.push(ts.SymbolFlags.Interface);
  }
  if (meaning & ts.SymbolFlags.TypeAlias) {
    meanings.push(ts.SymbolFlags.TypeAlias);
  }
  if (meaning & ts.SymbolFlags.TypeParameter) {
    meanings.push(ts.SymbolFlags.TypeParameter);
  }
  if (meaning & ts.SymbolFlags.Function) {
    meanings.push(ts.SymbolFlags.Function);
  }
  if (meaning & ts.SymbolFlags.Variable) {
    meanings.push(meaning & ts.SymbolFlags.Variable);
  }
  if (meaning & ts.SymbolFlags.Module) {
    meanings.push(meaning & ts.SymbolFlags.Module);
  }
  if (meaning & ts.SymbolFlags.ClassMember) {
    meanings.push(meaning & ts.SymbolFlags.ClassMember);
  }
  if (meaning & ts.SymbolFlags.Constructor) {
    meanings.push(ts.SymbolFlags.Constructor);
  }
  if (meaning & ts.SymbolFlags.EnumMember) {
    meanings.push(meaning);
  }
  return meanings;
}

function getArityForMeaningOfSymbol(symbol: ts.Symbol, meaning: ts.SymbolFlags): string {
  if (meaning & ts.SymbolFlags.Type) {
    const arity: number = symbol.declarations.reduce((a, decl) => Math.max(a, getArityOfDeclaration(decl)), 0);
    if (arity > 0) {
      return `\`${arity}`;
    }
  }
  return '';
}

function escapeUidFragment(text: string): string {
  text = text.replace(/=>|[{}<>]/g, s =>
    s === '=>' ? '~' :
    s === '{' ? '{{' :
    s === '}' ? '}}' :
    s === '<' ? '{' :
    s === '>' ? '}' :
    s);
  text = text.replace(/\b\s+\b/g, '-');
  text = text.replace(/\s+/g, '');
  return text;
}