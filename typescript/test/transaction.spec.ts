
import { Transaction } from '../src/transaction';
import { StandardItem, Item, ItemListImplementation, ItemList } from '../src/item';

export class TestTransaction {

    public static Scenarios(): void {

        describe('Given a transaction processor', () => {

            let transaction: Transaction;

            describe('When starting a new transaction', () => {

                let first: string;
                let second: string

                beforeEach(() => {
                    const itemList: ItemList = new ItemListImplementation();
                    transaction = new Transaction(itemList);
                    first = transaction.start();
                    second = transaction.start();
                });

                it('Should return a unique transaction number', () => {
                    expect(first).not.toEqual(second);
                });

            });

        });

        describe('Given a new transaction', () => {

            let transaction: Transaction;
            let transactionId: string;

            beforeEach(() => {
                const itemList: ItemList = new ItemListImplementation();
                transaction = new Transaction(itemList);
                transactionId = transaction.start();
            });

            describe('When requesting the total', () => {

                let total: number;

                beforeEach(() => {
                    total = transaction.total();
                });

                it('Should return zero.', () => {
                    expect(total).toEqual(0);
                });

            });

            describe('When removing an item', () => {

                const code: string = 'some by quantity item';
                let error: Error | null = null;

                beforeEach(() => {
                    try {
                        transaction.void(code);
                    } catch (exception) {
                        error = exception;
                    }
                });

                it('Then an error should be rasied.', () => {
                    expect(error).not.toBeNull();
                });

            });

        });

        describe('Given a transaction and a by quantity item', () => {

            let transaction: Transaction;
            const code: string = 'by quantity item';
            const price: number = 3.00;
            const item: Item = new StandardItem(
                code,
                'some random by quantity item.',
                'by quantity',
                price
            );

            beforeEach(() => {
                const itemList: ItemList = new ItemListImplementation();
                itemList.add(item);
                transaction = new Transaction(itemList);
            });

            describe('When scanning the item', () => {

                describe('a single time', () => {
                
                    let before: number;
                    let after: number;
                    let itemTotal: number;

                    beforeEach(() => {
                        before = transaction.quantity(code);
                        itemTotal = transaction.scan(code);
                        after = transaction.quantity(code);
                    });

                    it('Then the transaction should contain a quantity of one such item.', () => {
                        expect(before).toEqual(0);
                        expect(after).toEqual(1);
                    });

                    it('Then the item total should equal the item quantity times the item price.', () => {
                        expect(itemTotal).toEqual(after * price);
                    });

                });

                describe('twice', () => {
                
                    let before: number;
                    let after: number;
                    let itemTotal: number;

                    beforeEach(() => {
                        before = transaction.quantity(code);
                        transaction.scan(code);
                        itemTotal = transaction.scan(code);
                        after = transaction.quantity(code);
                    });

                    it('Then the transaction should contain a quantity of two such items.', () => {
                        expect(before).toEqual(0);
                        expect(after).toEqual(2);
                    });

                    it('Then the item total should be the quantity times price.', () => {
                        expect(itemTotal).toEqual(after * price);
                    });

                });

                describe('a single time with a weight', () => {

                    let error: Error | null = null;

                    beforeEach(() => {
                        try {
                            transaction.scan(code, 4.25);
                        } catch (exception) {
                            error = exception;                            
                        }
                    })

                    it('Then an error should be raised.', () => {
                        expect(error).not.toBeNull();
                    });

                });

            });

            describe('When voiding', () => {

                let before: number;
                let after: number;

                describe('the one and only item', () => {

                    beforeEach(() => {
                        transaction.scan(code);
                        before = transaction.quantity(code);
                        transaction.void(code);
                        after = transaction.quantity(code);
                    });
    
                    it('Then transaction should no longer have the item.', () => {
                        expect(before).toEqual(1);
                        expect(after).toEqual(0);
                    });

                });

                describe('one of the two items', () => {

                    beforeEach(() => {
                        transaction.scan(code);
                        transaction.scan(code);
                        before = transaction.quantity(code);
                        transaction.void(code);
                        after = transaction.quantity(code);
                    });
    
                    it('Then transaction should no longer have the item.', () => {
                        expect(before).toEqual(2);
                        expect(after).toEqual(1);
                    });
                });

            });

        });

        describe('Given a transaction and one or more by weight items', () => {

            let transaction: Transaction;
            const code: string = 'by weight item';
            const price: number = 1.25
            const item: Item = new StandardItem(
                code,
                'some random by weight item.',
                'by weight',
                price
            );

            beforeEach(() => {
                const itemList: ItemList = new ItemListImplementation();
                itemList.add(item);
                transaction = new Transaction(itemList);
            });

            describe('When scanning', () => {

                describe('without providing the weight', () => {

                    let error: Error | null = null;

                    beforeEach(() => {
                        try {
                            transaction.scan(code);
                        } catch (exception) {
                            error = exception;
                        }
                    });

                    it('Should raise an error.', () => {
                        expect(error).not.toBeNull();
                    })

                });

                describe('and providing a weight', () => {

                    const weight: number = 2.5;
                    let before: number;
                    let after: number;
                    let itemTotal: number;

                    describe('for a single package', () => {

                        beforeEach(() => {
                            before = transaction.quantity(code);
                            itemTotal = transaction.scan(code, weight);
                            after = transaction.quantity(code);
                        });
    
                        it('Then the transaction should contain the item.', () => {
                            expect(before).toEqual(0);
                            expect(after).toEqual(1);
                        });

                        it('Then the item total should be the weight times the price.', () => {
                            expect(itemTotal).toEqual(weight * price);
                        });

                    });

                    describe('for multiple packages', () => {

                        const otherItemWeight: number = 3.0;

                        beforeEach(() => {
                            before = transaction.quantity(code);
                            transaction.scan(code, weight);
                            itemTotal = transaction.scan(code, otherItemWeight);
                            after = transaction.quantity(code);
                        });
    
                        it('Then the transaction should contain the items.', () => {
                            expect(before).toEqual(0);
                            expect(after).toEqual(2);
                        });

                        it('Then the item total should be the weight of both items times the price.', () => {
                            expect(itemTotal).toEqual((weight + otherItemWeight) * price);
                        });

                    });

                });

            });

            describe('When voiding', () => {

                describe('without providing the weight', () => {

                    let error: Error | null = null;

                    beforeEach(() => {
                        transaction.scan(code, 1.999);
                        try {
                            transaction.void(code);
                        } catch (exception) {
                            error = exception;
                        }
                    });

                    it('Should raise an error.', () => {
                        expect(error).not.toBeNull();
                    })

                });

                describe('and providing a weight', () => {

                    let before: number;
                    let after: number;
                    let itemTotal: number;

                    describe('for a single package', () => {

                        beforeEach(() => {
                            itemTotal = transaction.scan(code, 2.5);
                            before = transaction.quantity(code);
                            transaction.void(code, 2.5);
                            after = transaction.quantity(code);
                        });
    
                        it('Then the transaction should no longer contain the item.', () => {
                            expect(before).toEqual(1);
                            expect(after).toEqual(0);
                        });

                    });

                    describe('for multiple packages', () => {

                        beforeEach(() => {
                            itemTotal = transaction.scan(code, 1);
                            itemTotal = transaction.scan(code, 2);
                            before = transaction.quantity(code);
                            transaction.void(code, 1);
                            after = transaction.quantity(code);
                        });
    
                        it('Then the quantity is reduced by one.', () => {
                            expect(before).toEqual(2);
                            expect(after).toEqual(1);
                        });

                    });

                });

            });

        });

    }

}
