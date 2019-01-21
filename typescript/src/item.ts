
export class Item {
    constructor(
        public readonly itemCode: string
    ) {
    }
}

export class ItemList {
    public add(item: Item): void {
        if ((item.itemCode || 'missing item code') === 'missing item code') {
            throw new Error('Missing required Item Code.');
        }
    }

    public contains(item: Item): boolean {
        return true;
    }
}