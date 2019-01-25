import { ItemList, ItemType } from './item';
import { DateRange } from './date';
import { Currency } from './currency';

export interface DiscountList {
    add(discount: Discount): void;
    includes(discount: Discount): boolean;
    get(date: Date, code: string): Discount | undefined;
}

export class DiscountItem {
    constructor(
        public readonly price: number,
        public readonly quantity: number,
        public readonly weight?: number
    ) { }
}

export interface Discount {
    readonly startDate: Date;
    readonly endDate: Date;
    readonly code: string;
    validate(itemList: ItemList): void;
    total(items: Array<DiscountItem>): number;
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

    public total(items: Array<DiscountItem>): number {
        let total: number = 0;

        items.forEach((item: DiscountItem): void => {
            const totalQuantity = item.weight === undefined
                ? item.quantity
                : item.quantity * item.weight;

            total += Math.min(item.price, this.price) * totalQuantity;
        });

        return total;
    }
}

type ItemSummary = {
    quantity: number;
    price: number;
}

const sumItems: (items: Array<DiscountItem>) => ItemSummary = (items: Array<DiscountItem>): ItemSummary => {
    let quantity: number = 0;
    let price: number = 0;

    items.forEach((item: DiscountItem): void => {
        quantity += item.quantity;
        price = item.price;
    });

    return {
        quantity: quantity,
        price: price
    };
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

    public total(items: Array<DiscountItem>): number {
        const item: ItemSummary = sumItems(items);
        const salePrice: number = Math.floor(item.quantity / this.quantity) * this.price * this.quantity;
        const regularPrice: number = (item.quantity % this.quantity) * item.price;
        return salePrice + regularPrice;
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

    public abstract total(items: Array<DiscountItem>): number;
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

    public total(items: Array<DiscountItem>): number {
        const item: ItemSummary = sumItems(items);
        const quantity: number = this.bulk + this.sale;
        const salePrice: number = item.price * (1 - this.percent);
        const regularCost: number = (item.quantity % quantity) * item.price;
        const saleCost: number = Math.floor(item.quantity / quantity) * ((this.bulk * item.price) + (this.sale * salePrice));
        return Currency.floor(regularCost + saleCost);
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

    public total(items: Array<DiscountItem>): number {
        const item: ItemSummary = sumItems(items);
        const over: number = item.quantity > this.limit
            ? (item.quantity - this.limit)
            : 0;
        const under: number = item.quantity - over;
        const quantity: number = this.bulk + this.sale;
        const salePrice: number = item.price * (1 - this.percent);
        const overCost: number = ((under % quantity) + over) * item.price;
        const bundleCost: number = (this.bulk * item.price) + (this.sale * salePrice);
        const bundles: number = Math.floor(under / quantity);
        return Currency.floor((bundles * bundleCost) + overCost);
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

    public total(items: Array<DiscountItem>): number {
        const item: ItemSummary = sumItems(items);
        const quantity: number = this.bulk + this.sale;
        const salePrice: number = item.price * this.price;
        const regularCost: number = (item.quantity % quantity) * item.price;
        const bundleCost: number = (this.bulk * item.price) + (this.sale * salePrice);
        const bundles: number = Math.floor(item.quantity / quantity);
        return regularCost + (bundles * bundleCost);
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

    public total(items: Array<DiscountItem>): number {
        const item: ItemSummary = sumItems(items);
        const over: number = item.quantity > this.limit
            ? (item.quantity - this.limit)
            : 0;
        const under: number = item.quantity - over;
        const quantity: number = this.bulk + this.sale;
        const overCost: number = ((under % quantity) + over) * item.price;
        const bundleCost: number = (this.bulk * item.price) + (this.sale * this.price);
        const bundles: number = Math.floor(under / quantity);
        return (bundles * bundleCost) + overCost;
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

    public total(items: Array<DiscountItem>): number {

        const sorted: Array<DiscountItem> = items.sort((red: DiscountItem, green: DiscountItem): number => {
            const redCost: number = red.quantity * (red.weight || 0) * red.price;
            const greenCost: number = green.quantity * (green.weight || 0) * green.price;
            if (redCost < greenCost) {
                return 1;
            } else if (redCost > greenCost) {
                return -1;
            } else {
                return 0
            }
        });

        let total: number = 0;

        sorted.forEach((item: DiscountItem, index: number): void => {
            const oneBasedIndex: number = (index + 1) % (this.bulk + this.sale);
            if (oneBasedIndex >= 1 && oneBasedIndex <= this.bulk) {
                total += item.quantity * (item.weight || 0) * item.price;
            } else {
                total += item.quantity * (item.weight || 0) * (item.price * (1 - this.percent));
            }
        });

        return Currency.floor(total);
    }

}

export class DiscountListImplementation implements DiscountList {

    constructor(private readonly itemList: ItemList) { }

    private readonly list: Array<Discount> = new Array<Discount>();

    public add(discount: Discount): void {
        discount.validate(this.itemList);

        const duplicates: Array<Discount> = this.matching(discount.code, discount.startDate, discount.endDate);

        if (duplicates.length > 0) {
            throw new Error('Duplicate or overlapping discount for ' + discount.code);
        }

        this.list.push(discount);
    }

    private matching(code: string, startDate: Date, endDate: Date): Array<Discount> {
        return this.list.filter((existing: Discount) => {
            const byCode: boolean = (existing.code === code);
            const byDate: boolean = (DiscountListImplementation.overlap(existing.startDate, existing.endDate, startDate, endDate));
            return byCode && byDate;
        });
    }

    public get(date: Date, code: string): Discount | undefined {
        const matching: Array<Discount> = this.matching(code, date, date);

        if (matching.length === 0) {
            return undefined;
        } else {
            return matching[0];
        }
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