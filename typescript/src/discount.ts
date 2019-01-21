
export interface Discount {
    readonly startDate: Date,
    readonly endDate: Date,
    readonly code: string,
}

export class StandardDiscount implements Discount {
    constructor(
        public readonly startDate: Date,
        public readonly endDate: Date,
        public readonly code: string,
        public readonly price: number
    ) {}
}

export class BulkFlatPriceDiscount implements Discount {
    constructor(
        public readonly startDate: Date,
        public readonly endDate: Date,
        public readonly code: string,
        public readonly quantity: number,
        public readonly price: number
    ) {}
}

export class UpSaleDiscount implements Discount {
    constructor(
        public readonly startDate: Date,
        public readonly endDate: Date,
        public readonly code: string,
        public readonly bulk: number,
        public readonly sale: number,
        public readonly percent: number
    ) {}
}

export class LimitedUpSaleDiscount extends UpSaleDiscount {
    constructor(
        readonly startDate: Date,
        readonly endDate: Date,
        readonly code: string,
        readonly bulk: number,
        readonly sale: number,
        readonly percent: number,
        public readonly limit: number
    ) {
        super(startDate, endDate, code, bulk, sale, percent);
    }
}

export class DiscountList {
    
    public add(discount: Discount): void {

    }

    public includes(discount: Discount): boolean {
        return true;
    }
}