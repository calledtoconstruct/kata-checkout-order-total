import { ItemList, ItemType, Item, Priced } from './item';
import { DateRange } from './date';
import { Currency } from './currency';

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
    readonly tag: string;
    readonly discountStartDate: Date;
    readonly discountEndDate: Date;
    readonly discountCode: string;
    validate(itemList: ItemList): Promise<void>;
    total(items: Array<DiscountItem>): number;
    getTypeName(): string;
}

const validateItemCode = (discount: Discount): void => {
    if (discount.discountCode.length === 0) {
        throw new Error('Invalid Item Code');
    }
};

const validateItemType = async (itemList: ItemList, code: string, type: ItemType): Promise<void> => {
    const item: (Item & Priced) | undefined = await itemList.get(code);

    if (item === undefined) {
        throw new Error('Requested Item Does Not Exist.');
    }

    if (item.itemType !== type) {
        throw new Error('Item Type Mismatch');
    }
};
const fallbackDateRange: DateRange = {
    startDate: new Date(2001, 1, 1, 8, 0, 0, 0),
    endDate: new Date(2001, 1, 31, 17, 0, 0, 0)
};

const validateItemDateRange: (discount: Discount) => void = (discount: Discount): void => {
    if ((discount.discountStartDate || fallbackDateRange.startDate).valueOf() >= (discount.discountEndDate || fallbackDateRange.endDate).valueOf()) {
        throw new Error('The end date must be after the start date.');
    }
};

export class StandardDiscount implements Discount {
    private static typeName = 'StandardDiscount';

    public readonly tag: string = StandardDiscount.typeName;

    constructor(
        public readonly discountStartDate: Date,
        public readonly discountEndDate: Date,
        public readonly discountCode: string,
        public readonly discountPrice: number
    ) { }

    public getTypeName(): string { return StandardDiscount.typeName; }

    public async validate(itemList: ItemList): Promise<void> {
        validateItemCode(this);
        await validateItemType(itemList, this.discountCode, 'by quantity');
        validateItemDateRange(this);
    }

    public total(items: Array<DiscountItem>): number {
        let total = 0;

        items.forEach((item: DiscountItem): void => {
            const totalQuantity = item.weight === undefined
                ? item.quantity
                : item.quantity * item.weight;

            total += Math.min(item.price, this.discountPrice) * totalQuantity;
        });

        return total;
    }
}

interface ItemSummary {
    quantity: number;
    price: number;
}

const sumItems: (items: Array<DiscountItem>) => ItemSummary = (items: Array<DiscountItem>): ItemSummary => {
    let quantity = 0;
    let price = 0;

    items.forEach((item: DiscountItem): void => {
        quantity += item.quantity;
        price = item.price;
    });

    return {
        quantity: quantity,
        price: price
    };
};

export class BulkFlatPriceDiscount implements Discount {
    private static typeName = 'BulkFlatPriceDiscount';

    public readonly tag: string = BulkFlatPriceDiscount.typeName;

    constructor(
        public readonly discountStartDate: Date,
        public readonly discountEndDate: Date,
        public readonly discountCode: string,
        public readonly discountBulk: number,
        public readonly discountPrice: number
    ) { }

    public getTypeName(): string { return BulkFlatPriceDiscount.typeName; }

    public async validate(itemList: ItemList): Promise<void> {
        validateItemCode(this);
        await validateItemType(itemList, this.discountCode, 'by quantity');
        validateItemDateRange(this);
    }

    public total(items: Array<DiscountItem>): number {
        const item: ItemSummary = sumItems(items);
        const salePrice: number = Math.floor(item.quantity / this.discountBulk) * this.discountPrice * this.discountBulk;
        const regularPrice: number = (item.quantity % this.discountBulk) * item.price;
        return salePrice + regularPrice;
    }
}

export interface Percent {
    discountPercent: number;
}

const validatePercentNotGreaterThanOneHundred: (percent: Percent) => void = (percent: Percent): void => {
    if (percent.discountPercent > 1) {
        throw new Error('Percent must be Less Than or Equal To One Hundred');
    }
};

const validatePercentNotEqualToZero: (percent: Percent) => void = (percent: Percent): void => {
    if (percent.discountPercent === 0) {
        throw new Error('Percent must be Greater Than Zero');
    }
};

export interface Limited {
    discountBulk: number;
    discountSale: number;
    discountLimit: number;
}

const validateLimitIsMultipleOfBulkAndSaleQuantity: (limited: Limited) => void = (limited: Limited): void => {
    if (limited.discountLimit === 0) {
        throw new Error('Limit must be Greater Than Zero');
    } else if ((limited.discountLimit % (limited.discountBulk + limited.discountSale)) !== 0) {
        throw new Error('Limit must be a Multiple of the Sum of the Bulk and Sale Quantities');
    }
};

export interface UpSale {
    readonly discountBulk: number;
    readonly discountSale: number;
}

const validateBulkQuantity: (upSale: UpSale) => void = (upSale: UpSale): void => {
    if (upSale.discountBulk < 1) {
        throw new Error('Bulk Quantity must be Greater Than Zero');
    }
};

const validateSaleQuantity: (upSale: UpSale) => void = (upSale: UpSale): void => {
    if (upSale.discountSale < 1) {
        throw new Error('Sale Quantity must be Greater Than Zero');
    }
};

const validateWholeNumberBulkQuantity: (upSale: UpSale) => void = (upSale: UpSale): void => {
    if (Math.floor(upSale.discountBulk) !== upSale.discountBulk) {
        throw new Error('Bulk Quantity must be a Whole Number');
    }
};

const validateWholeNumberSaleQuantity: (upSale: UpSale) => void = (upSale: UpSale): void => {
    if (Math.floor(upSale.discountSale) !== upSale.discountSale) {
        throw new Error('Sale Quantity must be a Whole Number');
    }
};

abstract class UpSaleDiscount implements Discount, UpSale {

    public readonly tag: string;

    constructor(
        public readonly discountStartDate: Date,
        public readonly discountEndDate: Date,
        public readonly discountCode: string,
        public readonly discountBulk: number,
        public readonly discountSale: number,
    ) {
        this.tag = this.getTypeName();
    }

    public abstract getTypeName(): string;

    public abstract validate(itemList: ItemList): Promise<void>;

    protected async validateItemType(itemList: ItemList, type: ItemType): Promise<void> {
        validateItemCode(this);
        validateWholeNumberBulkQuantity(this);
        validateBulkQuantity(this);
        validateWholeNumberSaleQuantity(this);
        validateSaleQuantity(this);
        await validateItemType(itemList, this.discountCode, type);
        validateItemDateRange(this);
    }

    public abstract total(items: Array<DiscountItem>): number;
}

export class UpSalePercentDiscount extends UpSaleDiscount implements Percent {
    private static readonly typeName: string = 'UpSalePercentDiscount';

    constructor(
        readonly discountStartDate: Date,
        readonly discountEndDate: Date,
        readonly discountCode: string,
        readonly discountBulk: number,
        readonly discountSale: number,
        readonly discountPercent: number
    ) {
        super(discountStartDate, discountEndDate, discountCode, discountBulk, discountSale);
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
        const quantity: number = this.discountBulk + this.discountSale;
        const salePrice: number = item.price * (1 - this.discountPercent);
        const regularCost: number = (item.quantity % quantity) * item.price;
        const saleCost: number = Math.floor(item.quantity / quantity) * ((this.discountBulk * item.price) + (this.discountSale * salePrice));
        return Currency.floor(regularCost + saleCost);
    }

}

export class LimitedUpSalePercentDiscount extends UpSaleDiscount implements Percent {
    private static readonly typeName: string = 'LimitedUpSalePercentDiscount';

    constructor(
        readonly discountStartDate: Date,
        readonly discountEndDate: Date,
        readonly discountCode: string,
        readonly discountBulk: number,
        readonly discountSale: number,
        readonly discountPercent: number,
        public readonly discountLimit: number
    ) {
        super(discountStartDate, discountEndDate, discountCode, discountBulk, discountSale);
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
        const over: number = item.quantity > this.discountLimit
            ? (item.quantity - this.discountLimit)
            : 0;
        const under: number = item.quantity - over;
        const quantity: number = this.discountBulk + this.discountSale;
        const salePrice: number = item.price * (1 - this.discountPercent);
        const overCost: number = ((under % quantity) + over) * item.price;
        const bundleCost: number = (this.discountBulk * item.price) + (this.discountSale * salePrice);
        const bundles: number = Math.floor(under / quantity);
        return Currency.floor((bundles * bundleCost) + overCost);
    }

}

export class UpSaleFlatPriceDiscount extends UpSaleDiscount {
    private static readonly typeName: string = 'UpSaleFlatPriceDiscount';

    constructor(
        readonly discountStartDate: Date,
        readonly discountEndDate: Date,
        readonly discountCode: string,
        readonly discountBulk: number,
        readonly discountSale: number,
        public readonly discountPrice: number
    ) {
        super(discountStartDate, discountEndDate, discountCode, discountBulk, discountSale);
    }

    public getTypeName(): string {
        return UpSaleFlatPriceDiscount.typeName;
    }

    public async validate(itemList: ItemList): Promise<void> {
        await super.validateItemType(itemList, 'by quantity');
    }

    public total(items: Array<DiscountItem>): number {
        const item: ItemSummary = sumItems(items);
        const quantity: number = this.discountBulk + this.discountSale;
        const salePrice: number = item.price * this.discountPrice;
        const regularCost: number = (item.quantity % quantity) * item.price;
        const bundleCost: number = (this.discountBulk * item.price) + (this.discountSale * salePrice);
        const bundles: number = Math.floor(item.quantity / quantity);
        return regularCost + (bundles * bundleCost);
    }

}

export class LimitedUpSaleFlatPriceDiscount extends UpSaleDiscount implements Limited {
    private static readonly typeName: string = 'LimitedUpSaleFlatPriceDiscount';

    constructor(
        readonly discountStartDate: Date,
        readonly discountEndDate: Date,
        readonly discountCode: string,
        readonly discountBulk: number,
        readonly discountSale: number,
        public readonly discountPrice: number,
        readonly discountLimit: number
    ) {
        super(discountStartDate, discountEndDate, discountCode, discountBulk, discountSale);
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
        const over: number = item.quantity > this.discountLimit
            ? (item.quantity - this.discountLimit)
            : 0;
        const under: number = item.quantity - over;
        const quantity: number = this.discountBulk + this.discountSale;
        const overCost: number = ((under % quantity) + over) * item.price;
        const bundleCost: number = (this.discountBulk * item.price) + (this.discountSale * this.discountPrice);
        const bundles: number = Math.floor(under / quantity);
        return (bundles * bundleCost) + overCost;
    }

}

export class UpSalePercentDiscountByWeight extends UpSaleDiscount implements Percent {
    private static readonly typeName: string = 'UpSalePercentDiscountByWeight';

    constructor(
        readonly discountStartDate: Date,
        readonly discountEndDate: Date,
        readonly discountCode: string,
        readonly discountBulk: number,
        readonly discountSale: number,
        readonly discountPercent: number
    ) {
        super(discountStartDate, discountEndDate, discountCode, discountBulk, discountSale);
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
                return 0;
            }
        });

        let total = 0;

        sorted.forEach((item: DiscountItem, index: number): void => {
            const oneBasedIndex: number = (index + 1) % (this.discountBulk + this.discountSale);
            if (oneBasedIndex >= 1 && oneBasedIndex <= this.discountBulk) {
                total += item.quantity * (item.weight || 0) * item.price;
            } else {
                total += item.quantity * (item.weight || 0) * (item.price * (1 - this.discountPercent));
            }
        });

        return Currency.floor(total);
    }

}

export class DiscountListImplementation implements DiscountList {

    constructor(private readonly itemList: ItemList) { }

    private readonly list: Array<Discount> = new Array<Discount>();

    private static overlap(firstStart: Date, firstEnd: Date, secondStart: Date, secondEnd: Date): boolean {
        if (firstStart < secondStart && firstEnd < secondStart) {
            return false;
        } else if (secondEnd < firstStart && secondEnd < firstEnd) {
            return false;
        }
        return true;
    }

    public async add(discount: Discount): Promise<void> {
        await discount.validate(this.itemList);

        const duplicates: Array<Discount> = this.matching(discount.discountCode, discount.discountStartDate, discount.discountEndDate);

        if (duplicates.length > 0) {
            throw new Error('Duplicate or overlapping discount for ' + discount.discountCode);
        }

        this.list.push(discount);
    }

    private matching(code: string, startDate: Date, endDate: Date): Array<Discount> {
        return this.list.filter((existing: Discount) => {
            const byCode: boolean = (existing.discountCode === code);
            const byDate: boolean = (DiscountListImplementation.overlap(existing.discountStartDate, existing.discountEndDate, startDate, endDate));
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

}

export class DiscountTypeFactory {

    private static readonly map = new Array<[string, (tagged: Discount) => Discount]>(
        ['StandardDiscount', (tagged: Discount): Discount => {
            const standardDiscount: StandardDiscount = <StandardDiscount>tagged;
            return new StandardDiscount(
                standardDiscount.discountStartDate,
                standardDiscount.discountEndDate,
                standardDiscount.discountCode,
                standardDiscount.discountPrice
            );
        }], ['BulkFlatPriceDiscount', (tagged: Discount): Discount => {
            const bulkFlatPriceDiscount: BulkFlatPriceDiscount = <BulkFlatPriceDiscount>tagged;
            return new BulkFlatPriceDiscount(
                bulkFlatPriceDiscount.discountStartDate,
                bulkFlatPriceDiscount.discountEndDate,
                bulkFlatPriceDiscount.discountCode,
                bulkFlatPriceDiscount.discountBulk,
                bulkFlatPriceDiscount.discountPrice
            );
        }], ['UpSaleFlatPriceDiscount', (tagged: Discount): Discount => {
            const upSaleFlatPriceDiscount: UpSaleFlatPriceDiscount = <UpSaleFlatPriceDiscount>tagged;
            return new UpSaleFlatPriceDiscount(
                upSaleFlatPriceDiscount.discountStartDate,
                upSaleFlatPriceDiscount.discountEndDate,
                upSaleFlatPriceDiscount.discountCode,
                upSaleFlatPriceDiscount.discountBulk,
                upSaleFlatPriceDiscount.discountSale,
                upSaleFlatPriceDiscount.discountPrice
            );
        }], ['UpSalePercentDiscount', (tagged: Discount): Discount => {
            const upSalePercentDiscount: UpSalePercentDiscount = <UpSalePercentDiscount>tagged;
            return new UpSalePercentDiscount(
                upSalePercentDiscount.discountStartDate,
                upSalePercentDiscount.discountEndDate,
                upSalePercentDiscount.discountCode,
                upSalePercentDiscount.discountBulk,
                upSalePercentDiscount.discountSale,
                upSalePercentDiscount.discountPercent
            );
        }], ['UpSalePercentDiscountByWeight', (tagged: Discount): Discount => {
            const upSalePercentDiscountByWeight: UpSalePercentDiscountByWeight = <UpSalePercentDiscountByWeight>tagged;
            return new UpSalePercentDiscountByWeight(
                upSalePercentDiscountByWeight.discountStartDate,
                upSalePercentDiscountByWeight.discountEndDate,
                upSalePercentDiscountByWeight.discountCode,
                upSalePercentDiscountByWeight.discountBulk,
                upSalePercentDiscountByWeight.discountSale,
                upSalePercentDiscountByWeight.discountPercent
            );
        }], ['LimitedUpSalePercentDiscount', (tagged: Discount): Discount => {
            const limitedUpSalePercentDiscount: LimitedUpSalePercentDiscount = <LimitedUpSalePercentDiscount>tagged;
            return new LimitedUpSalePercentDiscount(
                limitedUpSalePercentDiscount.discountStartDate,
                limitedUpSalePercentDiscount.discountEndDate,
                limitedUpSalePercentDiscount.discountCode,
                limitedUpSalePercentDiscount.discountBulk,
                limitedUpSalePercentDiscount.discountSale,
                limitedUpSalePercentDiscount.discountPercent,
                limitedUpSalePercentDiscount.discountLimit
            );
        }], ['LimitedUpSaleFlatPriceDiscount', (tagged: Discount): Discount => {
            const limitedUpSaleFlatPriceDiscount: LimitedUpSaleFlatPriceDiscount = <LimitedUpSaleFlatPriceDiscount>tagged;
            return new LimitedUpSaleFlatPriceDiscount(
                limitedUpSaleFlatPriceDiscount.discountStartDate,
                limitedUpSaleFlatPriceDiscount.discountEndDate,
                limitedUpSaleFlatPriceDiscount.discountCode,
                limitedUpSaleFlatPriceDiscount.discountBulk,
                limitedUpSaleFlatPriceDiscount.discountSale,
                limitedUpSaleFlatPriceDiscount.discountPrice,
                limitedUpSaleFlatPriceDiscount.discountLimit
            );
        }]
    );

    public get(tagged: Discount): Discount {
        let output: Discount | null = null;
        DiscountTypeFactory.map.forEach(element => {
            if (element[0] === tagged.tag) {
                output = element[1](tagged);
            }
        });
        if (output == null) {
            throw Error('Failed while attempting to deserialize a Discount object.');
        }
        return output;
    }
}
