
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

        describe('Given a transaction and a by quantity item', () => {

            let transaction: Transaction;
            let before: number;
            let after: number;
            let itemTotal: number;
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
                    before = transaction.quantity(code);
                    itemTotal = transaction.scan(code);
                    after = transaction.quantity(code);
                });

                it('Should contain the item.', () => {
                    expect(before).toEqual(0);
                    expect(after).toEqual(1);
                });

            });

        });

    }

}
