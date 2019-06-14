
/** @public */
export declare class ClassWithAmbiguity {
    constructor(x: string);
    __constructor(x: string): void;
    static __constructor(x: string): void;
}

/** @public */
export declare enum EnumAndNamespace {
    Apple = 0,
    Banana = 1,
    Coconut = 2
}

/** @public */
export declare namespace EnumAndNamespace {
    export function parseEnum(value: string): EnumAndNamespace;
    export namespace NestedNamespace {
        const test: number;
    }
}

declare interface ForgottenExport {
    a: string;
}

/** @public */
export declare function functionWithUnexportedType(forgottenExport: ForgottenExport): void;

/** @public */
export declare function ThreeKinds(x: string): ThreeThings;

/** @public */
export declare namespace ThreeKinds {
    const x: number;
}

/** @public */
export declare interface ThreeThings {
    x: number;
}

/** @public */
export declare function veryComplexSignature(options: {
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
} & Partial<ForgottenExport>): void;

export { }
