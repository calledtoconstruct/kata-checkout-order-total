
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

        });

        describe('Given a transaction and a single by quantity item', () => {

            let transaction: Transaction;
            const code: string = 'by quantity item';
            const item: Item = new StandardItem(
                code,
                'some random by quantity item.',
                'by quantity',
                3.00
            );

            describe('When scanning the item', () => {

                beforeEach(() => {
                    const itemList: ItemList = new ItemListImplementation();
                    itemList.add(item);
                    transaction = new Transaction(itemList);
                });

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

                });

                describe('twice', () => {
                
                    let before: number;
                    let after: number;
                    let itemTotal: number;

                    beforeEach(() => {
                        before = transaction.quantity(code);
                        itemTotal = transaction.scan(code);
                        itemTotal = transaction.scan(code);
                        after = transaction.quantity(code);
                    });

                    it('Then the transaction should contain a quantity of two such items.', () => {
                        expect(before).toEqual(0);
                        expect(after).toEqual(2);
                    });

                });

            });

        });

        describe('Given a transaction and one or more by weight item', () => {

            let transaction: Transaction;
            const code: string = 'by weight item';
            const item: Item = new StandardItem(
                code,
                'some random by weight item.',
                'by weight',
                1.25
            );

            describe('When scanning the item', () => {

                beforeEach(() => {
                    const itemList: ItemList = new ItemListImplementation();
                    itemList.add(item);
                    transaction = new Transaction(itemList);
                });

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

                    let before: number;
                    let after: number;
                    let itemTotal: number;

                    beforeEach(() => {
                        before = transaction.quantity(code);
                        itemTotal = transaction.scan(code, 2.5);
                        after = transaction.quantity(code);
                    });

                    it('Then the transaction should contain the item.', () => {
                        expect(before).toEqual(0);
                        expect(after).toEqual(1);
                    });

                });

            });

        });

    }

}
