
import { DiscountList, Discount } from "./discount";
import { sendRequest } from "./api.client";

export class DiscountListClient implements DiscountList {

    async get(date: Date, code: string): Promise<Discount | undefined> {
        try {
            return await sendRequest(document.baseURI.replace('8080', '8082') + `discount/${date.valueOf()}/${code}`);
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
