
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

export class DiscountList {
    
    public add(discount: Discount): void {

    }

    public includes(discount: Discount): boolean {
        return true;
    }
}