
export interface ItemList {
    add(item: Item): void;
    includes(item: Item): boolean;
}

export interface Item {
    readonly code: string | null;
    readonly type: 'by quantity' | 'by weight' | null;
    validate(): void;
}

export class StandardItem implements Item {
    constructor(
        public readonly code: string | null,
        public readonly description: string | null,
        public readonly type: 'by quantity' | 'by weight' | null,
        public readonly price: number | null
    ) {
    }

    public validate(): void {
        if ((this.code || null) === null) {
            throw new Error('Missing required Item Code.');
        }
        if ((this.description || null) === null) {
            throw new Error('Missing required Description.');
        }
        if ((this.type || null) === null) {
            throw new Error('Missing required Type.');
        }
        if ((this.price || null) === null) {
            throw new Error('Missing required Price.');
        }
    }
}

export class ItemListImplementation implements ItemList {
    private readonly list: Array<Item> = new Array<Item>();

    public add(item: Item): void {
        item.validate();
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