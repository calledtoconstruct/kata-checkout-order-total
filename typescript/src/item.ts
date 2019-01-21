
export interface ItemList {
    add(item: Item): void;
    includes(item: Item): boolean;
}

export class Item {
    constructor(
        public readonly code: string | null,
        public readonly description: string | null,
        public readonly type: 'by quantity' | 'by weight' | null,
        public readonly price: number | null
    ) {
    }
}

export class ItemListImplementation implements ItemList {
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
        const copy = this.list.splice(0);
        copy.filter((value: Item): boolean => {
            return value.code !== item.code;
        }).forEach((value: Item): void => {
            this.list.push(value);
        });
        this.list.push(item);
    }

    public includes(item: Item): boolean {
        return this.list.indexOf(item) !== -1;
    }
}