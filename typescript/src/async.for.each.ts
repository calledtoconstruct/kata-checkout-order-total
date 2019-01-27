
export const asyncForEach = async <Target>(source: Array<Target>, action: (value: Target) => Promise<void>): Promise<void> => {
    for (let key in source) {
        const value: Target = source[key];
        await action(value);
    }
};