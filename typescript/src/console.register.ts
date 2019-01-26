import { Transaction } from "./transaction";
import { ItemListImplementation, ItemList, Item, StandardItem } from "./item";
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

  private addByWeightItem(item: Item, weight: number): number {
    try {
      const itemTotal: number = this.transaction.scan(item.code, weight);
      console.log('Added ' + weight + ' lbs of ' + item.type + ' item ' + item.code);
      return itemTotal;
    } catch (error) {
      if (error instanceof Error) {
        console.log(error.message);
      }
    }
    return 0;
  }
  
  private addByQuantityItem(item: Item): number {
    try {
      const itemTotal: number = this.transaction.scan(item.code);
      console.log('Added ' + item.type + ' item ' + item.code);
      return itemTotal;
    } catch (error) {
      if (error instanceof Error) {
        console.log(error.message);
      }
    }
    return 0;
  }

  private processWeight(item: Item): (weight: number) => void {
    return (weight: number): void => {
      this.addByWeightItem(item, weight);
      this.next();
    }
  }
  
  private processCode(code: string): void {
    if (code === 'done') {
      const total: number = this.transaction.total();
      console.log('Thank you for your patronage.  ' + total + ' was charged to your account.');
      this.input.close();
    } else {
      let item: Item;
      try {
        item = itemList.get(code);
        if (item.type === 'by weight') {
          console.log('Weigh the item: ');
          this.input.question('>', (weight: number) => this.processWeight(item)(weight));
        } else {
          this.addByQuantityItem(item);
          this.next();
        }
      } catch (error) {
        if (error instanceof Error) {
          console.log(error.message);
        }
      }
    }
  }

  public next(): void {
    const total: number = this.transaction.total();
  
    console.log('The current transaction total is: ' + total);
    console.log('Scan the next item: ');
  
    this.input.question('>', (code: string) => this.processCode(code));
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