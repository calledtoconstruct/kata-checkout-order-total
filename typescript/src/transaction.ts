
import * as uuid from 'uuid';
import { Item, ItemList } from './item';

class TransactionItem {

    constructor(private readonly item: Item) { }

    private howMany: number = 1;

    public code(): string {
        // TODO: Clean this up!  Given the Lint Rules, TypeScript will enforce this.
        if (this.item.code === null) {
            throw new Error('Invalid item: Missing required item code.');
        }
        return this.item.code;
    }

    public quantity(): number {
        return this.howMany;
    }

    public total(): number {
        return 0;
    }
}

export class Transaction {

    constructor(
        private readonly itemList: ItemList
    ) {
    }

    private readonly item: Array<TransactionItem> = new Array<TransactionItem>();

    public start(): string {
        const uniqueId: string = uuid();
        return 'transaction ' + uniqueId;
    }

    public scan(code: string): number {
        const scanned: Item = this.itemList.get(code);
        const transactionItem: TransactionItem = new TransactionItem(scanned);
        this.item.push(transactionItem);
        return 0;
    }

    public total(): number {
        return 0;
    }

    public quantity(code: string): number {
        const matching: Array<TransactionItem> = this.item.filter((value: TransactionItem): boolean => value.code() === code);
        let count: number = 0;
        matching.forEach((value: TransactionItem): void => {
            count += value.quantity()
        });
        return count;
    }

}