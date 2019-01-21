
export class Item {
    constructor(
        public readonly code: string | null
    ) {
    }
}

export class ItemList {
    private readonly list: Array<Item> = new Array<Item>();

    public add(item: Item): void {
        if ((item.code || 'missing item code') === 'missing item code') {
            throw new Error('Missing required Item Code.');
        }
        this.list.push(item);
    }

    public includes(item: Item): boolean {
        return this.list.indexOf(item) !== -1;
    }
}