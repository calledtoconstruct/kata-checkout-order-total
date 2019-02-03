
import { DiscountList, Discount, DiscountTypeFactory } from "./discount";
import { sendRequest } from "./api.client";
import * as env from './env';
import { Typed, TypeFactory } from "./typed";

export class DiscountListClient implements DiscountList {
    private readonly typefactory: TypeFactory<Discount> = new DiscountTypeFactory();

    async get(date: Date, code: string): Promise<Discount | undefined> {
        try {
            const typed: Typed<Discount> = await sendRequest(`http://${env.BASE_URL}:${env.DISCOUNT_API_PORT}/discount/${date.valueOf()}/${code}`);
            const discount: Discount = this.typefactory.make(typed);
            return discount;
        }
        catch (error) {
            if (error === 404) {
                return undefined;
            }
            throw error;
        }
    }

    async add(_: Discount): Promise<void> {
        throw new Error("Method not implemented.");
    }

    async includes(_: Discount): Promise<boolean> {
        throw new Error("Method not implemented.");
    }

}
