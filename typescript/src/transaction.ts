
import { v4 as uuid } from 'uuid';
import { Item, Priced, ItemList } from './item';
import { DiscountList, Discount, DiscountItem } from './discount';
import { asyncForEach } from './async.for.each';

class TransactionItem {

    constructor(
        private readonly item: Item & Priced,
        private readonly itemWeight?: number
    ) { }

    private howMany: number = 1;

    public code(): string {
        return this.item.itemCode;
    }

    public price(): number {
        return this.item.itemPrice;
    }

    public weight(): number | undefined {
        return this.itemWeight;
    }

    public quantity(): number {
        return this.howMany;
    }

    public decrement(): void {

    }

    public total(): number {
        const totalQuantity: number = this.itemWeight === undefined
            ? this.howMany
            : this.howMany * this.itemWeight;

        return totalQuantity * this.item.itemPrice;
    }
}

export class Transaction {

    constructor(
        private readonly itemList: ItemList,
        private readonly discountList: DiscountList
    ) {
    }

    private date: Date;
    private transaction: string;

    private readonly item: Array<TransactionItem> = new Array<TransactionItem>();

    public start(): string {
        this.date = new Date();

        const uniqueId: string = uuid();
        this.transaction = 'transaction ' + uniqueId;

        return this.transaction;
    }

    private static validateType(scanned: Item, weight?: number): void {
        if (scanned.itemType === 'by weight' && weight === undefined) {
            throw new Error('Weight is required for this type of item.');
        } else if (scanned.itemType === 'by quantity' && weight !== undefined) {
            throw new Error('Weight is not required for this type of item.');
        }
    }

    public async scan(code: string, weight?: number): Promise<number> {
        const scanned: (Item & Priced) | undefined = await this.itemList.get(code);

        if (scanned === undefined) {
            throw new Error('Requested Item Does Not Exist.');
        }

        Transaction.validateType(scanned, weight);

        const transactionItem: TransactionItem = new TransactionItem(scanned, weight);
        
        this.item.push(transactionItem);

        return await this.itemTotal(code);
    }

    private async itemTotal(code: string): Promise<number> {
        const discount: Discount | undefined = await this.discountList.get(this.date, code);
        const related: Array<TransactionItem> = this.items(code);

        if (discount === undefined) {
            let total: number = 0;

            related.forEach((transactionItem: TransactionItem): void => {
                total += transactionItem.total();
            });

            return total;
        } else {
            const items: Array<DiscountItem> = new Array<DiscountItem>();

            related.forEach((item: TransactionItem): void => {
                const discountItem: DiscountItem = new DiscountItem(item.price(), item.quantity(), item.weight());
                items.push(discountItem);
            });

            return discount.total(items);
        }
    }

    private items(code: string): Array<TransactionItem> {
        return this.item.filter((value: TransactionItem): boolean => value.code() === code);
    }

    public async void(code: string, weight?: number): Promise<void> {
        const scanned: (Item & Priced) | undefined = await this.itemList.get(code);

        if (scanned === undefined) {
            throw new Error('Requested Item Does Not Exist.');
        }

        Transaction.validateType(scanned, weight);

        const items: Array<TransactionItem> = this.items(code);

        if (items.length === 0) {
            throw new Error('Item could not be removed because it does not exist.');
        }

        const item: TransactionItem = items[0];

        if (item.quantity() > 1) {
            item.decrement();
        } else if (item.quantity() === 1) {
            const index: number = this.item.indexOf(item);
            this.item.splice(index, 1);
        } else {
            throw new Error('Invalid item quantity.');
        }
    }

    public async total(): Promise<number> {
        let code: Array<string> = new Array<string>();

        this.item.forEach((item: TransactionItem): void => {
            const itemCode: string = item.code();
            const index: number = code.indexOf(itemCode);

            if (index < 0) {
                code.push(itemCode);
            }
        });

        let total: number = 0;

        await asyncForEach(code, async (itemCode: string): Promise<void> => {
            const itemTotal = await this.itemTotal(itemCode);
            total += itemTotal;
        });

        return total;
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
