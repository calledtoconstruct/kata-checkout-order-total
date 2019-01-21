
export interface ItemList {
    get(code: string): Item;
    add(item: Item): void;
    includes(item: Item): boolean;
}

export type ItemType = 'by quantity' | 'by weight' | null;

export interface Item {
    readonly code: string | null;
    readonly type: ItemType;
    validate(): void;
}

export class StandardItem implements Item {
    constructor(
        public readonly code: string | null,
        public readonly description: string | null,
        public readonly type: ItemType,
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

    public get(code: string): Item {
        const items = this.list.filter((value: Item): boolean => value.code === code);
        if (items.length === 0) {
            throw new Error('Requested Item Does Not Exist');
        }
        return items[0];
    }
}