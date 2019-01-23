
import { Transaction } from '../src/transaction';

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

    }

}
