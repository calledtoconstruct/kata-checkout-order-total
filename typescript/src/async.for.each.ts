
export const asyncForEach = <Target>(source: Array<Target>, action: (value: Target) => Promise<void>): Promise<void> => {
    return new Promise<void>((resolve: () => void) => {
        for (let index = 0; index < source.length; ++index) {
            const value: Target = source[index];
            action(value);
        }
        resolve();
    });
};
