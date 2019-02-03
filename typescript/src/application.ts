
import { ItemList } from "./item";
import { DiscountList } from "./discount";
import { Transaction } from "./transaction";
import { ItemListClient } from "./item.list.client";
import { DiscountListClient } from "./discount.list.client";

const itemList: ItemList = new ItemListClient();
const discountList: DiscountList = new DiscountListClient();
const transaction: Transaction = new Transaction(itemList, discountList);

transaction.start();

window.addEventListener('load', (): void => {
    const form: HTMLElement | null = document.getElementById('scanner');
    if (form instanceof HTMLElement) {
        form.addEventListener('submit', async (event: Event): Promise<void> => {
            event.preventDefault();
            const code: HTMLElement | null = document.getElementById('code');
            if (code instanceof HTMLElement) {
                if (code instanceof HTMLInputElement) {
                    const value: string = code.value;
                    await transaction.scan(value);
                    const totalElement: HTMLElement | null = document.getElementById('total');
                    if (totalElement instanceof HTMLElement) {
                        const total: number = await transaction.total();
                        totalElement.innerHTML = total.toString();
                    }
                }
            }
        });
    }
});
