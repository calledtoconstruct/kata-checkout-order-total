
export const asyncForEach = async <Target>(source: Array<Target>, action: (value: Target) => Promise<void>): Promise<void> => {
    for (let index = 0; index < source.length; ++index) {
        const value: Target = source[index];
        await action(value);
    }
};
