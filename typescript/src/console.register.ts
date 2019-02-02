import { Transaction } from "./transaction";
import { ItemListImplementation, ItemList, Item, StandardItem, Priced } from "./item";
import { DiscountListImplementation, DiscountList, Discount, StandardDiscount, UpSalePercentDiscountByWeight } from "./discount";

const readline = require('readline');

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
      const itemTotal: number = await this.transaction.scan(item.code, weight);
      console.log('Added ' + weight + ' lbs of ' + item.type + ' item ' + item.code);
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
      const itemTotal: number = await this.transaction.scan(item.code);
      console.log('Added ' + item.type + ' item ' + item.code);
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
          if (item.type === 'by weight') {
            console.log('Weigh the item: ');
            this.input.question('>', async (weight: number) => await this.processWeight(item)(weight));
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

const itemList: ItemList = new ItemListImplementation();

const items: Array<Item> = [
  new StandardItem('dog food', 'A 32oz package of premium dog food.', 'by quantity', 5.00),
  new StandardItem('cat food', 'A 12oz can of premium cat food.', 'by quantity', 1.25),
  new StandardItem('ground beef', 'Ground Chuck', 'by weight', 2.00)
];

items.forEach((item: Item): void => {
  try {
    itemList.add(item);
  } catch (error) {
    if (error instanceof Error) {
      console.log(error.message);
    }
  }
});

const startDate: Date = new Date(new Date().valueOf() - 10);
const endDate: Date = new Date(new Date().valueOf() + 10);

const discountList: DiscountList = new DiscountListImplementation(itemList);

const discounts: Array<Discount> = [
  new StandardDiscount(startDate, endDate, 'cat food', 1.00),
  new UpSalePercentDiscountByWeight(startDate, endDate, 'ground beef', 2, 1, .5)
];

discounts.forEach((discount: Discount): void => {
  try {
    discountList.add(discount);
  } catch (error) {
    if (error instanceof Error) {
      console.log(error.message);
    }
  }
});

const transaction: Transaction = new Transaction(itemList, discountList);

const application: Application = new Application(transaction);

application.next();