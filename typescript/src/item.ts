
export interface ItemList {
    get(code: string): Item & Priced;
    add(item: Item): void;
    includes(item: Item): Promise<boolean>;
}

export type ItemType = 'by quantity' | 'by weight';

export interface Item {
    readonly code: string;
    readonly type: ItemType;
    validate(): void;
}

export interface Priced {
    readonly price: number;
}

export class StandardItem implements Item, Priced {
    constructor(
        public readonly code: string,
        public readonly description: string,
        public readonly type: ItemType,
        public readonly price: number
    ) {
    }

    public validate(): void {
        if (this.code === '') {
            throw new Error('Missing required Item Code.');
        }
        if (this.description === '') {
            throw new Error('Missing required Description.');
        }
        if (this.type !== 'by quantity' && this.type !== 'by weight') {
            throw new Error('Missing required Type.');
        }
        if (this.price === 0) {
            throw new Error('Missing required Price.');
        }
    }
}

export class ItemListImplementation implements ItemList {
    private readonly list: Array<Item & Priced> = new Array<Item & Priced>();

    public add(item: Item & Priced): void {
        item.validate();
        const copy = this.list.splice(0);
        copy.filter((value: Item): boolean => {
            return value.code !== item.code;
        }).forEach((value: Item & Priced): void => {
            this.list.push(value);
        });
        this.list.push(item);
    }

    public async includes(item: Item & Priced): Promise<boolean> {
        return Promise.resolve(this.list.indexOf(item) !== -1);
    }

    public get(code: string): Item & Priced {
        const items = this.list.filter((value: Item): boolean => value.code === code);
        if (items.length === 0) {
            throw new Error('Requested Item Does Not Exist');
        }
        return items[0];
    }
}