import { TestScenario, Parameterized } from "./parameterized";
import {
    Discount,
    StandardDiscount,
    BulkFlatPriceDiscount,
    UpSalePercentDiscount,
    LimitedUpSalePercentDiscount,
    UpSaleFlatPriceDiscount,
    LimitedUpSaleFlatPriceDiscount,
    UpSalePercentDiscountByWeight,
    DiscountTypeFactory
} from "../src/discount";

export class TestDiscountSerialization {

    public static scenarios(): void {

        const serializationScenarios = new Parameterized<DiscountSerializationScenario, TestScenario<DiscountSerializationScenario>>([
            { description: 'standard discount', target: new StandardDiscountSerializationScenario() },
            { description: 'bulk flat price discount', target: new BulkFlatPriceDiscountSerializationScenario() },
            { description: 'up sale percent discount', target: new UpSalePercentDiscountSerializationScenario() },
            { description: 'limited up sale percent discount', target: new LimitedUpSalePercentDiscountSerializationScenario() },
            { description: 'up sale flat price discount', target: new UpSaleFlatPriceDiscountSerializationScenario() },
            { description: 'limited up sale flat price discount', target: new LimitedUpSaleFlatPriceDiscountSerializationScenario() },
            { description: 'up sale percent discount by weight', target: new UpSalePercentDiscountByWeightSerializationScenario() }
        ]);

        describe('Given the discount type factory', () => {

            const typeFactory = new DiscountTypeFactory();

            serializationScenarios.forEach().describe('when serializing', (scenario: TestScenario<DiscountSerializationScenario>) => {

                const description: string = scenario.description;
                const output: string = JSON.stringify(scenario.target.instance);

                describe(description, () => {

                    it('should produce typed json', () => {
                        expect(output).toEqual(scenario.target.text);
                    });

                });

            });

            serializationScenarios.forEach().describe('when deserializing', (scenario: TestScenario<DiscountSerializationScenario>) => {

                const description: string = scenario.description;
                const tagged: Discount = <Discount>JSON.parse(scenario.target.text);
                const output: Discount = typeFactory.get(tagged);

                describe(description, () => {

                    it('should produce fully populated instance', () => {
                        expect(output.total).not.toBeUndefined();
                        expect(output.validate).not.toBeUndefined();
                        expect(output.getTypeName).not.toBeUndefined();
                        expect(output.getTypeName()).toEqual(scenario.target.instance.getTypeName());
                    });

                });

            });

        });

    }

}

class DiscountSerializationScenario {
    protected constructor(
        public readonly instance: Discount,
        public readonly text: string
    ) { }
}

const startDate: Date = new Date(new Date(2019, 1, 1));
const startDateAsString = startDate.toISOString();
const endDate: Date = new Date(new Date(2019, 2, 1));
const endDateAsString = endDate.toISOString();
const code: string = 'cat food';
const quantity: number = 3;
const bulk: number = 2;
const sale: number = 1;
const percent: number = 0.25;
const price: number = 1.00;
const limit: number = 3;

class StandardDiscountSerializationScenario extends DiscountSerializationScenario {
    constructor() {
        const discount: StandardDiscount = new StandardDiscount(startDate, endDate, code, price);
        const text: string = `{"discountStartDate":"${startDateAsString}","discountEndDate":"${endDateAsString}","discountCode":"cat food","discountPrice":1,"tag":"StandardDiscount"}`;
        super(discount, text);
    }
}

class BulkFlatPriceDiscountSerializationScenario extends DiscountSerializationScenario {
    constructor() {
        const discount: BulkFlatPriceDiscount = new BulkFlatPriceDiscount(startDate, endDate, code, quantity, price);
        const text: string = `{"discountStartDate":"${startDateAsString}","discountEndDate":"${endDateAsString}","discountCode":"cat food","discountBulk":3,"discountPrice":1,"tag":"BulkFlatPriceDiscount"}`;
        super(discount, text);
    }
}

class UpSalePercentDiscountSerializationScenario extends DiscountSerializationScenario {
    constructor() {
        const discount: UpSalePercentDiscount = new UpSalePercentDiscount(startDate, endDate, code, bulk, sale, percent);
        const text: string = `{"discountStartDate":"${startDateAsString}","discountEndDate":"${endDateAsString}","discountCode":"cat food","discountBulk":2,"discountSale":1,"tag":"UpSalePercentDiscount","discountPercent":0.25}`;
        super(discount, text);
    }
}

class LimitedUpSalePercentDiscountSerializationScenario extends DiscountSerializationScenario {
    constructor() {
        const discount: LimitedUpSalePercentDiscount = new LimitedUpSalePercentDiscount(startDate, endDate, code, bulk, sale, percent, limit);
        const text: string = `{"discountStartDate":"${startDateAsString}","discountEndDate":"${endDateAsString}","discountCode":"cat food","discountBulk":2,"discountSale":1,"tag":"LimitedUpSalePercentDiscount","discountPercent":0.25,"discountLimit":3}`;
        super(discount, text);
    }
}

class UpSaleFlatPriceDiscountSerializationScenario extends DiscountSerializationScenario {
    constructor() {
        const discount: UpSaleFlatPriceDiscount = new UpSaleFlatPriceDiscount(startDate, endDate, code, bulk, sale, price);
        const text: string = `{"discountStartDate":"${startDateAsString}","discountEndDate":"${endDateAsString}","discountCode":"cat food","discountBulk":2,"discountSale":1,"tag":"UpSaleFlatPriceDiscount","discountPrice":1}`;
        super(discount, text);
    }
}

class LimitedUpSaleFlatPriceDiscountSerializationScenario extends DiscountSerializationScenario {
    constructor() {
        const discount: LimitedUpSaleFlatPriceDiscount = new LimitedUpSaleFlatPriceDiscount(startDate, endDate, code, bulk, sale, price, limit);
        const text: string = `{"discountStartDate":"${startDateAsString}","discountEndDate":"${endDateAsString}","discountCode":"cat food","discountBulk":2,"discountSale":1,"tag":"LimitedUpSaleFlatPriceDiscount","discountPrice":1,"discountLimit":3}`;
        super(discount, text);
    }
}

class UpSalePercentDiscountByWeightSerializationScenario extends DiscountSerializationScenario {
    constructor() {
        const discount: UpSalePercentDiscountByWeight = new UpSalePercentDiscountByWeight(startDate, endDate, code, bulk, sale, price);
        const text: string = `{"discountStartDate":"${startDateAsString}","discountEndDate":"${endDateAsString}","discountCode":"cat food","discountBulk":2,"discountSale":1,"tag":"UpSalePercentDiscountByWeight","discountPercent":1}`;
        super(discount, text);
    }
}
