import { Transaction } from "./transaction";
import { ItemList, Item, Priced } from "./item";
import { DiscountList } from "./discount";
import { ItemListClient } from "./item.list.client";
import { DiscountListClient } from "./discount.list.client";

import readline = require('readline');

class Application {

  private readonly input = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });

  constructor(
    private readonly transaction: Transaction
  ) {
    this.input.on('close', () => {
      process.exit();
    });
  }

  private async addByWeightItem(item: Item, weight: number): Promise<number> {
    try {
      const itemTotal: number = await this.transaction.scan(item.itemCode, weight);
      console.log('Added ' + weight + ' lbs of ' + item.itemType + ' item ' + item.itemCode);
      return itemTotal;
    } catch (error) {
      if (error instanceof Error) {
        console.log(error.message);
      }
    }
    return 0;
  }

  private async addByQuantityItem(item: Item): Promise<number> {
    try {
      const itemTotal: number = await this.transaction.scan(item.itemCode);
      console.log('Added ' + item.itemType + ' item ' + item.itemCode);
      return itemTotal;
    } catch (error) {
      if (error instanceof Error) {
        console.log(error.message);
      }
    }
    return 0;
  }

  private processWeight(item: Item & Priced): (weight: number) => void {
    return async (weight: number): Promise<void> => {
      await this.addByWeightItem(item, weight);
      await this.next();
    }
  }

  private async processCode(code: string): Promise<void> {
    if (code === 'done') {
      const total: number = await this.transaction.total();
      console.log('Thank you for your patronage.  ' + total + ' was charged to your account.');
      this.input.close();
    } else {
      try {
        const item: (Item & Priced) | undefined = await itemList.get(code);
        if (item !== undefined) {
          if (item.itemType === 'by weight') {
            console.log('Weigh the item: ');
            this.input.question('>', async (weight: string) => await this.processWeight(item)(parseFloat(weight)));
          } else {
            await this.addByQuantityItem(item);
            await this.next();
          }
        }
      } catch (error) {
        if (error instanceof Error) {
          console.log(error.message);
        }
      }
    }
  }

  public async next(): Promise<void> {
    const total: number = await this.transaction.total();

    console.log('The current transaction total is: ' + total);
    console.log('Scan the next item: ');

    this.input.question('>', async (code: string) => await this.processCode(code));
  }
}

const itemList: ItemList = new ItemListClient();

const discountList: DiscountList = new DiscountListClient();

const transaction: Transaction = new Transaction(itemList, discountList);

transaction.start();

const application: Application = new Application(transaction);

application.next();