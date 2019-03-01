
export interface ItemList {
    get(itemCode: string): Promise<(Item & Priced) | undefined>;
    add(item: Item): Promise<void>;
    includes(item: Item): Promise<boolean>;
}

export type ItemType = 'by quantity' | 'by weight';

export interface Item {
    readonly itemCode: string;
    readonly itemType: ItemType;
    validate(): void;
}

export interface Priced {
    readonly itemPrice: number;
}

export class StandardItem implements Item, Priced {

    public readonly tag: string;

    constructor(
        public readonly itemCode: string,
        public readonly itemDescription: string,
        public readonly itemType: ItemType,
        public readonly itemPrice: number
    ) {
        this.tag = this.itemType === 'by quantity'
            ? 'ByQuantityItem'
            : 'ByWeightItem'
    }

    public validate(): void {
        if (this.itemCode === '') {
            throw new Error('Missing required Item Code.');
        }
        if (this.itemDescription === '') {
            throw new Error('Missing required Description.');
        }
        if (this.itemType !== 'by quantity' && this.itemType !== 'by weight') {
            throw new Error('Missing required Type.');
        }
        if (this.itemPrice === 0) {
            throw new Error('Missing required Price.');
        }
    }
}

export class ItemListImplementation implements ItemList {
    private readonly list: Array<Item & Priced> = new Array<Item & Priced>();

    public async add(item: Item & Priced): Promise<void> {
        item.validate();
        const copy = this.list.splice(0);
        copy.filter((value: Item): boolean => {
            return value.itemCode !== item.itemCode;
        }).forEach((value: Item & Priced): void => {
            this.list.push(value);
        });
        this.list.push(item);
        return Promise.resolve();
    }

    public async includes(item: Item & Priced): Promise<boolean> {
        return this.list.indexOf(item) !== -1;
    }

    public async get(itemCode: string): Promise<(Item & Priced) | undefined> {
        const items = this.list.filter((value: Item): boolean => value.itemCode === itemCode);
        if (items.length === 0) {
            return undefined;
        }
        return Promise.resolve(items[0]);
    }
}
