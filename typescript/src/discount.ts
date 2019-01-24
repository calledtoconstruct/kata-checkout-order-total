import { ItemList, ItemType } from './item';
import { DateRange } from './date';

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

const validateItemCode: (discount: Discount) => void = (discount: Discount): void => {
    if (discount.code.length === 0) {
        throw new Error('Invalid Item Code');
    }
};

const validateItemType: (itemList: ItemList, code: string, type: ItemType) => void = (itemList: ItemList, code: string, type: ItemType): void => {
    const item = itemList.get(code);
    if (item.type !== type) {
        throw new Error('Item Type Mismatch');
    }
};
const fallbackDateRange: DateRange = {
    startDate: new Date(2001, 1, 1, 8, 0, 0, 0),
    endDate: new Date(2001, 1, 31, 17, 0, 0, 0)
};

const validateItemDateRange: (discount: Discount) => void = (discount: Discount): void => {
    if ((discount.startDate || fallbackDateRange.startDate).valueOf() >= (discount.endDate || fallbackDateRange.endDate).valueOf()) {
        throw new Error('The end date must be after the start date.');
    }
};

export class StandardDiscount implements Discount {
    constructor(
        public readonly startDate: Date,
        public readonly endDate: Date,
        public readonly code: string,
        public readonly price: number
    ) { }

    public validate(itemList: ItemList): void {
        validateItemCode(this);
        validateItemType(itemList, this.code, 'by quantity');
        validateItemDateRange(this);
    }
}

export class BulkFlatPriceDiscount implements Discount {
    constructor(
        public readonly startDate: Date,
        public readonly endDate: Date,
        public readonly code: string,
        public readonly quantity: number,
        public readonly price: number
    ) { }
    public validate(itemList: ItemList): void {
        validateItemCode(this);
        validateItemType(itemList, this.code, 'by quantity');
        validateItemDateRange(this);
    }
}

export interface UpSale {
    readonly bulk: number,
    readonly sale: number
}

const validateBulkQuantity: (upSale: UpSale) => void = (upSale: UpSale): void => {
    if (upSale.bulk < 1) {
        throw new Error('Bulk Quantity must be Greater Than Zero');
    }
};

const validateSaleQuantity: (upSale: UpSale) => void = (upSale: UpSale): void => {
    if (upSale.sale < 1) {
        throw new Error('Sale Quantity must be Greater Than Zero');
    }
};

const validateWholeNumberBulkQuantity: (upSale: UpSale) => void = (upSale: UpSale): void => {
    if (Math.floor(upSale.bulk) !== upSale.bulk) {
        throw new Error('Bulk Quantity must be a Whole Number');
    }
};

const validateWholeNumberSaleQuantity: (upSale: UpSale) => void = (upSale: UpSale): void => {
    if (Math.floor(upSale.sale) !== upSale.sale) {
        throw new Error('Sale Quantity must be a Whole Number');
    }
};

abstract class UpSaleDiscount implements Discount, UpSale {
    constructor(
        public readonly startDate: Date,
        public readonly endDate: Date,
        public readonly code: string,
        public readonly bulk: number,
        public readonly sale: number
    ) { }
    public abstract validate(itemList: ItemList): void;

    protected validateItemType(itemList: ItemList, type: ItemType): void {
        validateItemCode(this);
        validateWholeNumberBulkQuantity(this);
        validateBulkQuantity(this);
        validateWholeNumberSaleQuantity(this);
        validateSaleQuantity(this);
        validateItemType(itemList, this.code, type);
        validateItemDateRange(this);
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

    constructor(private readonly itemList: ItemList) { }

    public add(discount: Discount): void {
        discount.validate(this.itemList);

        const duplicates: Array<Discount> = this.matching(discount);

        if (duplicates.length > 0) {
            throw new Error('Duplicate or overlapping discount for ' + discount.code);
        }

        this.list.push(discount);
    }

    private matching(discount: Discount): Array<Discount> {
        return this.list.filter((existing: Discount) => {
            const code: boolean = (existing.code === discount.code);
            const date: boolean = (DiscountListImplementation.overlap(existing.startDate, existing.endDate, discount.startDate, discount.endDate));
            return code && date;
        });
    }

    public includes(discount: Discount): boolean {
        return this.list.indexOf(discount) !== -1;
    }

    private static overlap(firstStart: Date, firstEnd: Date, secondStart: Date, secondEnd: Date): boolean {
        if (firstStart < secondStart && firstEnd < secondStart) {
            return false;
        } else if (secondEnd < firstStart && secondEnd < firstEnd) {
            return false;
        }
        return true;
    }
}