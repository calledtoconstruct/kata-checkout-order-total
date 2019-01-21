/* global describe, it, expect, beforeEach */

import { ItemList, Item, ItemListImplementation, StandardItem, ItemType } from '../src/item';

import {
    Discount,
    StandardDiscount,
    DiscountList,
    BulkFlatPriceDiscount,
    UpSalePercentDiscount,
    LimitedUpSalePercentDiscount,
    UpSaleFlatPriceDiscount,
    LimitedUpSaleFlatPriceDiscount,
    UpSalePercentDiscountByWeight,
    DiscountListImplementation
} from '../src/discount';

interface Parameterized<T> {
    it(description: string, func: (value: T) => void): void;
    describe(description: string, func: (value: T) => void): void;
}

class Parameters<T> {
    constructor(public readonly cases: Array<T>) {}
    public forEach(): Parameterized<T> { 
        return {
            it: (description: string, func: (value: T) => void): void => {
                this.cases.forEach((cs: T): void => {
                    it(description, (): void => {
                        func(cs);
                    });     
                });
            },
            describe: (description: string, func: (value: T) => void): void => {
                this.cases.forEach((cs: T): void => {
                    describe(description, (): void => {
                        func(cs);
                    });     
                });
            }
        };
    }
}

describe('Given a collection of Items', () => {
    let itemList: ItemList;

    beforeEach(() => {
        itemList = new ItemListImplementation();
    });

    describe('And a new item containing an Item Code, Description, Type, and Price', () => {
        const item = new StandardItem(
            'random item code',
            'random description',
            'by quantity',
            3.0
        );

        describe('When adding the item', () => {

            beforeEach(() => {
                itemList.add(item);
            });

            it('Should exist in the list', () => {
                const result = itemList.includes(item);
                expect(result).toBe(true);
            });

        });

        describe('When adding the item again', () => {
            const otherItem = new StandardItem(
                'random item code',
                'random description',
                'by quantity',
                5.0
            );

            beforeEach(() => {
                itemList.add(item);
                itemList.add(otherItem);
            });

            it('the first item should not exist in the list', () => {
                const result = itemList.includes(item);
                expect(result).toBe(false);
            });

            it('the other item should exist in the list', () => {
                const result = itemList.includes(otherItem);
                expect(result).toBe(true);
            });

        });

    });

    const invalidItems: Parameters<[string, Item]> = new Parameters<[string, Item]>([
        ['item code', new StandardItem(null, 'random description', 'by weight', 3.0)],
        ['description', new StandardItem('random item code', null, 'by quantity', 3.0)],
        ['type', new StandardItem('random item code', 'random description', null, 3.0)],
        ['price', new StandardItem('random item code', 'random description', 'by weight', null)]
    ]);

    invalidItems.forEach().describe('And an invalid item When adding it, it', (item: [string, Item]) => {
        let error: Error | null = null;

        beforeEach(() => {
            try {
                itemList.add(item[1]);
                fail();
            } catch (caught) {
                error = caught;
            }
        });

        it('Should reject the item because it is missing ' + item[0], () => {
            expect(error).not.toBeNull()
        });

        it('Should not be added to the list', () => {
            const result = itemList.includes(item[1]);
            expect(result).toBe(false);
        });

    });
    
});

describe('Given a collection of Pricing Rules', () => {
    class FakeByQuantityItem implements Item {
        public readonly code: string = 'by quantity item';
        public readonly type: ItemType = 'by quantity';
        public validate(): void {}
    }

    class FakeByWeightItem implements Item {
        public readonly code: string = 'by weight item';
        public readonly type: ItemType = 'by weight';
        public validate(): void {}
    }

    class FakeItemList implements ItemList {
        public add(item: Item): void {
            throw new Error("Method not implemented.");
        }
        public includes(item: Item): boolean {
            throw new Error("Method not implemented.");
        }
        public get(code: string): Item {
            if (code === 'by quantity item') {
                return new FakeByQuantityItem();
            } else if (code === 'by weight item') {
                return new FakeByWeightItem();
            } else throw new Error('invalid test');
        }
    }

    let discountList: DiscountList;

    beforeEach(() => {
        const itemList: ItemList = new FakeItemList()
        discountList = new DiscountListImplementation(itemList);
    });

    const validDiscounts: Parameters<[string, Discount]> = new Parameters<[string, Discount]>([
        ['standard discount', new StandardDiscount(new Date(), new Date(), 'by quantity item', 1.0)],
        ['bulk flat price', new BulkFlatPriceDiscount(new Date(), new Date(), 'by quantity item', 3, 5.0)],
        ['up sale percent discount', new UpSalePercentDiscount(new Date(), new Date(), 'by quantity item', 2, 1, 0.5)],
        ['limited up sale percent discount', new LimitedUpSalePercentDiscount(new Date(), new Date(), 'by quantity item', 3, 1, 1, 8)],
        ['up sale flat price discount', new UpSaleFlatPriceDiscount(new Date(), new Date(), 'by quantity item', 2, 1, 1.25)],
        ['limited up sale flat price discount', new LimitedUpSaleFlatPriceDiscount(new Date(), new Date(), 'by quantity item', 3, 2, 1, 10)],
        ['up sale percent discount by weight', new UpSalePercentDiscountByWeight(new Date(), new Date(), 'by weight item', 2, 1, .5)]
    ]);

    validDiscounts.forEach().describe('And a valid discount When adding it', (item: [string, Discount]) => {

        beforeEach(() => {
            discountList.add(item[1]);
        });

        it('Should be added to the list', () => {
            const result = discountList.includes(item[1]);
            expect(result).toBe(true);
        });

    });

    const invalidDiscountsWithItemTypeMismatch: Parameters<[string, Discount]> = new Parameters<[string, Discount]>([
        ['standard discount item type mismatch', new StandardDiscount(new Date(), new Date(), 'by weight item', 1.0)],
        ['bulk flat price item type mismatch', new BulkFlatPriceDiscount(new Date(), new Date(), 'by weight item', 3, 5.0)],
        ['up sale percent discount item type mismatch', new UpSalePercentDiscount(new Date(), new Date(), 'by weight item', 2, 1, 0.5)],
        ['limited up sale percent discount item type mismatch', new LimitedUpSalePercentDiscount(new Date(), new Date(), 'by weight item', 3, 1, 1, 8)],
        ['up sale flat price discount item type mismatch', new UpSaleFlatPriceDiscount(new Date(), new Date(), 'by weight item', 2, 1, 1.25)],
        ['limited up sale flat price discount item type mismatch', new LimitedUpSaleFlatPriceDiscount(new Date(), new Date(), 'by weight item', 3, 2, 1, 10)],
        ['up sale percent discount by weight item type mismatch', new UpSalePercentDiscountByWeight(new Date(), new Date(), 'by quantity item', 2, 1, .5)],
        ['standard discount no matching item', new StandardDiscount(new Date(), new Date(), 'no matching item', 1.0)],
        ['bulk flat price no matching item', new BulkFlatPriceDiscount(new Date(), new Date(), 'no matching item', 3, 5.0)],
        ['up sale percent discount no matching item', new UpSalePercentDiscount(new Date(), new Date(), 'no matching item', 2, 1, 0.5)],
        ['limited up sale percent discount no matching item', new LimitedUpSalePercentDiscount(new Date(), new Date(), 'no matching item', 3, 1, 1, 8)],
        ['up sale flat price discount no matching item', new UpSaleFlatPriceDiscount(new Date(), new Date(), 'no matching item', 2, 1, 1.25)],
        ['limited up sale flat price discount no matching item', new LimitedUpSaleFlatPriceDiscount(new Date(), new Date(), 'no matching item', 3, 2, 1, 10)],
        ['up sale percent discount by weight no matching item', new UpSalePercentDiscountByWeight(new Date(), new Date(), 'no matching item', 2, 1, .5)]
    ]);

    invalidDiscountsWithItemTypeMismatch.forEach().describe('And an invalid discount When adding it', (item: [string, Discount]) => {
        let error: Error | null = null;

        beforeEach(() => {
            try {
                discountList.add(item[1]);
            } catch (exception) {
                error = exception;
            }
        });

        it(item[0] + ' Should raise an error', () => {
            expect(error).not.toBeNull();
        });

        it(item[0] + ' Should not be added to the list', () => {
            const result = discountList.includes(item[1]);
            expect(result).toBe(false);
        });

    });

});