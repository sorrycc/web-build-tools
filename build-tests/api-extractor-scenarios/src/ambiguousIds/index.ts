// Copyright (c) Microsoft Corporation. All rights reserved. Licensed under the MIT license.
// See LICENSE in the project root for license information.

/** @public */
export class ClassWithAmbiguity {
  public constructor(x: string) {
  }

  public __constructor(x: string): void {
  }

  public static __constructor(x: string): void {
  }
}

/** @public */
export enum EnumAndNamespace {
  Apple,
  Banana,
  Coconut
}

/** @public */
export namespace EnumAndNamespace {
  export function parseEnum(value: string): EnumAndNamespace {
    return EnumAndNamespace.Apple;
  }

  export namespace NestedNamespace {
    export const test: number = 123;
  }
}

/** @public */
export function ThreeKinds(x: string): ThreeThings {
  return { x: 123 };
}

/** @public */
export interface ThreeThings {
  x: number;
}

/** @public */
export namespace ThreeKinds {
  export const x: number = 321;
}

interface ForgottenExport {
  a: string;
}

/** @public */
export function functionWithUnexportedType(forgottenExport: ForgottenExport): void {
}

/** @public */
export function veryComplexSignature(options: {
    args: {
      [name: string]: string | boolean;
    };
    buildErrorIconPath?: string;
    buildSuccessIconPath?: string;
    distFolder: string;
    isRedundantBuild?: boolean;
    jestEnabled?: boolean;
    libAMDFolder?: string;
    libES6Folder?: string;
    libESNextFolder?: string;
    libFolder: string;
    onTaskEnd?: (taskName: string, duration: number[], error?: any) => void;
    onTaskStart?: (taskName: string) => void;
    packageFolder: string;
    production: boolean;
    properties?: {
      [key: string]: any;
    };
    relogIssues?: boolean;
    rootPath: string;
    shouldWarningsFailBuild: boolean;
    showToast?: boolean;
    srcFolder: string;
    tempFolder: string;
    verbose: boolean;
  } & Partial<ForgottenExport>): void {
}
