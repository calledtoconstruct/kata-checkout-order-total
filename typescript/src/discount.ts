import { ItemList, ItemType, Item, Priced } from './item';
import { DateRange } from './date';
import { Currency } from './currency';
import { Typed, TypeFactory } from './typed';

export interface DiscountList {
    add(discount: Discount): Promise<void>;
    includes(discount: Discount): Promise<boolean>;
    get(date: Date, code: string): Promise<Discount | undefined>;
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
    validate(itemList: ItemList): Promise<void>;
    total(items: Array<DiscountItem>): number;
    getTypeName(): string;
}

const validateItemCode = (discount: Discount): void => {
    if (discount.code.length === 0) {
        throw new Error('Invalid Item Code');
    }
};

const validateItemType = async (itemList: ItemList, code: string, type: ItemType): Promise<void> => {
    const item: (Item & Priced) | undefined = await itemList.get(code);

    if (item === undefined) {
        throw new Error('Requested Item Does Not Exist.');
    }

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
    private static typeName: string = "StandardDiscount";

    constructor(
        public readonly startDate: Date,
        public readonly endDate: Date,
        public readonly code: string,
        public readonly price: number
    ) { }

    public getTypeName(): string { return StandardDiscount.typeName; }

    public async validate(itemList: ItemList): Promise<void> {
        validateItemCode(this);
        await validateItemType(itemList, this.code, 'by quantity');
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
    private static typeName: string = "BulkFlatPriceDiscount";

    constructor(
        public readonly startDate: Date,
        public readonly endDate: Date,
        public readonly code: string,
        public readonly quantity: number,
        public readonly price: number
    ) { }

    public getTypeName(): string { return BulkFlatPriceDiscount.typeName; }

    public async validate(itemList: ItemList): Promise<void> {
        validateItemCode(this);
        await validateItemType(itemList, this.code, 'by quantity');
        validateItemDateRange(this);
    }

    public total(items: Array<DiscountItem>): number {
        const item: ItemSummary = sumItems(items);
        const salePrice: number = Math.floor(item.quantity / this.quantity) * this.price * this.quantity;
        const regularPrice: number = (item.quantity % this.quantity) * item.price;
        return salePrice + regularPrice;
    }
}

export interface Percent {
    percent: number;
}

const validatePercentNotGreaterThanOneHundred: (percent: Percent) => void = (percent: Percent): void => {
    if (percent.percent > 1) {
        throw new Error('Percent must be Less Than or Equal To One Hundred');
    }
};

const validatePercentNotEqualToZero: (percent: Percent) => void = (percent: Percent): void => {
    if (percent.percent === 0) {
        throw new Error('Percent must be Greater Than Zero');
    }
};

export interface Limited {
    bulk: number;
    sale: number;
    limit: number;
}

const validateLimitIsMultipleOfBulkAndSaleQuantity: (limited: Limited) => void = (limited: Limited): void => {
    if (limited.limit === 0) {
        throw new Error('Limit must be Greater Than Zero');
    } else if ((limited.limit % (limited.bulk + limited.sale)) !== 0) {
        throw new Error('Limit must be a Multiple of the Sum of the Bulk and Sale Quantities');
    }
};

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

    public abstract getTypeName(): string;

    public abstract async validate(itemList: ItemList): Promise<void>;

    protected async validateItemType(itemList: ItemList, type: ItemType): Promise<void> {
        validateItemCode(this);
        validateWholeNumberBulkQuantity(this);
        validateBulkQuantity(this);
        validateWholeNumberSaleQuantity(this);
        validateSaleQuantity(this);
        await validateItemType(itemList, this.code, type);
        validateItemDateRange(this);
    }

    public abstract total(items: Array<DiscountItem>): number;
}

export class UpSalePercentDiscount extends UpSaleDiscount implements Percent {
    private static readonly typeName: string = "UpSalePercentDiscount";

    constructor(
        readonly startDate: Date,
        readonly endDate: Date,
        readonly code: string,
        readonly bulk: number,
        readonly sale: number,
        readonly percent: number
    ) {
        super(startDate, endDate, code, bulk, sale);
    }

    public getTypeName(): string {
        return UpSalePercentDiscount.typeName;
    }

    public async validate(itemList: ItemList): Promise<void> {
        validatePercentNotEqualToZero(this);
        validatePercentNotGreaterThanOneHundred(this);
        await super.validateItemType(itemList, 'by quantity');
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

export class LimitedUpSalePercentDiscount extends UpSaleDiscount implements Percent {
    private static readonly typeName: string = "LimitedUpSalePercentDiscount";

    constructor(
        readonly startDate: Date,
        readonly endDate: Date,
        readonly code: string,
        readonly bulk: number,
        readonly sale: number,
        readonly percent: number,
        public readonly limit: number
    ) {
        super(startDate, endDate, code, bulk, sale);
    }

    public getTypeName(): string {
        return LimitedUpSalePercentDiscount.typeName;
    }

    public async validate(itemList: ItemList): Promise<void> {
        validatePercentNotEqualToZero(this);
        validatePercentNotGreaterThanOneHundred(this);
        await super.validateItemType(itemList, 'by quantity');
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
    private static readonly typeName: string = "UpSaleFlatPriceDiscount";

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

    public getTypeName(): string {
        return UpSaleFlatPriceDiscount.typeName;
    }

    public async validate(itemList: ItemList): Promise<void> {
        await super.validateItemType(itemList, 'by quantity');
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

export class LimitedUpSaleFlatPriceDiscount extends UpSaleDiscount implements Limited {
    private static readonly typeName: string = "LimitedUpSaleFlatPriceDiscount";

    constructor(
        readonly startDate: Date,
        readonly endDate: Date,
        readonly code: string,
        readonly bulk: number,
        readonly sale: number,
        public readonly price: number,
        readonly limit: number
    ) {
        super(startDate, endDate, code, bulk, sale);
    }

    public getTypeName(): string {
        return LimitedUpSaleFlatPriceDiscount.typeName;
    }

    public async validate(itemList: ItemList): Promise<void> {
        await super.validateItemType(itemList, 'by quantity');
        validateLimitIsMultipleOfBulkAndSaleQuantity(this);
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

export class UpSalePercentDiscountByWeight extends UpSaleDiscount implements Percent {
    private static readonly typeName: string = "UpSalePercentDiscountByWeight";

    constructor(
        readonly startDate: Date,
        readonly endDate: Date,
        readonly code: string,
        readonly bulk: number,
        readonly sale: number,
        readonly percent: number
    ) {
        super(startDate, endDate, code, bulk, sale);
    }

    public getTypeName(): string {
        return UpSalePercentDiscountByWeight.typeName;
    }

    public async validate(itemList: ItemList): Promise<void> {
        validatePercentNotEqualToZero(this);
        validatePercentNotGreaterThanOneHundred(this);
        await super.validateItemType(itemList, 'by weight');
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

    public async add(discount: Discount): Promise<void> {
        await discount.validate(this.itemList);

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

    public async get(date: Date, code: string): Promise<Discount | undefined> {
        const matching: Array<Discount> = this.matching(code, date, date);

        if (matching.length === 0) {
            return undefined;
        } else {
            return matching[0];
        }
    }

    public async includes(discount: Discount): Promise<boolean> {
        return Promise.resolve(this.list.indexOf(discount) !== -1);
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

export class DiscountTypeFactory implements TypeFactory<Discount> {
    private readonly discounts: any = {
        StandardDiscount: (from: Discount): Discount => {
            const source: StandardDiscount = <StandardDiscount>from;
            return new StandardDiscount(source.startDate, source.endDate, source.code, source.price);
        },
        BulkFlatPriceDiscount: (from: Discount): Discount => {
            const source: BulkFlatPriceDiscount = <BulkFlatPriceDiscount>from;
            return new BulkFlatPriceDiscount(source.startDate, source.endDate, source.code, source.quantity, source.price);
        }
    };

    public type(instance: Discount): Typed<Discount> {
        const name: string = instance.getTypeName();
        this.get(name);
        return new Typed<Discount>(name, instance);
    }

    public make(from: Typed<Discount>): Discount {
        const factory: (from: Discount) => Discount = this.get(from.type);
        return factory(from.thing);
    }

    private get(name: string): (from: Discount) => Discount {
        const factory: (from: Discount) => Discount = this.discounts[name];
        if (factory === undefined) {
            throw new Error(`Type ${name} is not buildable.`);
        }
        return factory;
    }
}
