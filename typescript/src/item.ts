
export class Item {
    constructor(
        public readonly code: string | null,
        public readonly description: string | null,
        public readonly type: 'by quantity' | 'by weight' | null,
        public readonly price: number | null
    ) {
    }
}

export class ItemList {
    private readonly list: Array<Item> = new Array<Item>();

    public add(item: Item): void {
        if ((item.code || null) === null) {
            throw new Error('Missing required Item Code.');
        }
        if ((item.description || null) === null) {
            throw new Error('Missing required Description.');
        }
        if ((item.type || null) === null) {
            throw new Error('Missing required Type.');
        }
        if ((item.price || null) === null) {
            throw new Error('Missing required Price.');
        }
        this.list.push(item);
    }

    public includes(item: Item): boolean {
        return this.list.indexOf(item) !== -1;
    }
}