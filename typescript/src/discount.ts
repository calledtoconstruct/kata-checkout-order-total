import { ItemList, ItemType } from "./item";

export interface DiscountList {
    add(discount: Discount): void;
    includes(discount: Discount): boolean;
}

export interface Discount {
    readonly startDate: Date;
    readonly endDate: Date;
    readonly code: string;
    validate(itemList: ItemList): void;
}

const validateItemType: (itemList: ItemList, code: string, type: ItemType) => void = (itemList: ItemList, code: string, type: ItemType): void => {
    const item = itemList.get(code);
    if (item.type !== type) {
        throw new Error('Item Type Mismatch');
    }
}

export class StandardDiscount implements Discount {
    constructor(
        public readonly startDate: Date,
        public readonly endDate: Date,
        public readonly code: string,
        public readonly price: number
    ) {}

    public validate(itemList: ItemList): void {
        validateItemType(itemList, this.code, 'by quantity');
    }
}

export class BulkFlatPriceDiscount implements Discount {
    constructor(
        public readonly startDate: Date,
        public readonly endDate: Date,
        public readonly code: string,
        public readonly quantity: number,
        public readonly price: number
    ) {}
    public validate(itemList: ItemList): void {
        validateItemType(itemList, this.code, 'by quantity');
    }
}

abstract class UpSaleDiscount implements Discount {
    constructor(
        public readonly startDate: Date,
        public readonly endDate: Date,
        public readonly code: string,
        public readonly bulk: number,
        public readonly sale: number
    ) {}
    public abstract validate(itemList: ItemList): void;

    protected validateItemType(itemList: ItemList, type: ItemType): void {
        validateItemType(itemList, this.code, type);
    }
}

export class UpSalePercentDiscount extends UpSaleDiscount {
    constructor(
        readonly startDate: Date,
        readonly endDate: Date,
        readonly code: string,
        readonly bulk: number,
        readonly sale: number,
        public readonly percent: number
    ) {
        super(startDate, endDate, code, bulk, sale);
    }
    public validate(itemList: ItemList): void {
        super.validateItemType(itemList, 'by quantity');
    }
}

export class LimitedUpSalePercentDiscount extends UpSaleDiscount {
    constructor(
        readonly startDate: Date,
        readonly endDate: Date,
        readonly code: string,
        readonly bulk: number,
        readonly sale: number,
        public readonly percent: number,
        public readonly limit: number
    ) {
        super(startDate, endDate, code, bulk, sale);
    }
    public validate(itemList: ItemList): void {
        super.validateItemType(itemList, 'by quantity');
    }
}

export class UpSaleFlatPriceDiscount extends UpSaleDiscount {
    constructor(
        readonly startDate: Date,
        readonly endDate: Date,
        readonly code: string,
        readonly bulk: number,
        readonly sale: number,
        public readonly price: number
    ) {
        super(startDate, endDate, code, bulk, sale);
    }
    public validate(itemList: ItemList): void {
        super.validateItemType(itemList, 'by quantity');
    }
}

export class LimitedUpSaleFlatPriceDiscount extends UpSaleDiscount {
    constructor(
        readonly startDate: Date,
        readonly endDate: Date,
        readonly code: string,
        readonly bulk: number,
        readonly sale: number,
        public readonly price: number,
        public readonly limit: number        
    ) {
        super(startDate, endDate, code, bulk, sale);
    }
    public validate(itemList: ItemList): void {
        super.validateItemType(itemList, 'by quantity');
    }
}

export class UpSalePercentDiscountByWeight extends UpSaleDiscount {
    constructor(
        readonly startDate: Date,
        readonly endDate: Date,
        readonly code: string,
        readonly bulk: number,
        readonly sale: number,
        public readonly percent: number
    ) {
        super(startDate, endDate, code, bulk, sale);
    }
    public validate(itemList: ItemList): void {
        super.validateItemType(itemList, 'by weight');
    }
}

export class DiscountListImplementation implements DiscountList {
    private readonly list: Array<Discount> = new Array<Discount>();

    constructor(private readonly itemList: ItemList) {}
    
    public add(discount: Discount): void {
        discount.validate(this.itemList);
        this.list.push(discount);
    }

    public includes(discount: Discount): boolean {
        return this.list.indexOf(discount) !== -1;
    }
}