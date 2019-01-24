
import { Transaction } from '../src/transaction';
import { StandardItem } from '../src/item';

export class TestTransaction {

    public static Scenarios(): void {

        describe('Given a transaction processor', () => {

            let transaction: Transaction;

            describe('When starting a new transaction', () => {

                let first: string;
                let second: string

                beforeEach(() => {
                    transaction = new Transaction();
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
                transaction = new Transaction();
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
            let itemTotal: number;
            const item = new StandardItem(
                'by quantity item',
                'some random by quantity item.', 
                'by quantity',
                3.00
            );

            describe('When scanning the item', () => {

                beforeEach(() => {
                    transaction = new Transaction();
                    itemTotal = transaction.scan(item);
                });

                it('Should contain the item.', () => {
                    const result = transaction.includes(item);
                    expect(result).toBe(true);
                });

            });

        });

    }

}
