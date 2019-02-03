import { TestScenario, Parameterized } from "./parameterized";
import { Discount, StandardDiscount, DiscountTypeFactory, BulkFlatPriceDiscount } from "../src/discount";
import { Typed, TypeFactory } from "../src/typed";

export class TestDiscountTypeFactory {

    public static scenarios(): void {

        const serializationScenarios = new Parameterized<DiscountSerializationScenario, TestScenario<DiscountSerializationScenario>>([
            { description: 'standard discount', target: new StandardDiscountSerializationScenario() },
            { description: 'bulk flat price discount', target: new BulkFlatPriceDiscountSerializationScenario() }
        ]);

        describe('Given the discount type factory', () => {

            const discountTypeFactory: TypeFactory<Discount> = new DiscountTypeFactory();

            serializationScenarios.forEach().describe('when serializing', (scenario: TestScenario<DiscountSerializationScenario>) => {

                const description: string = scenario.description;
                const typed: Typed<Discount> = discountTypeFactory.type(scenario.target.instance);
                const output: string = JSON.stringify(typed);

                describe(description, () => {

                    it('should produce typed json', () => {
                        expect(output).toEqual(scenario.target.text);
                    });

                });

            });

            serializationScenarios.forEach().describe('when deserializing', (scenario: TestScenario<DiscountSerializationScenario>) => {

                const description: string = scenario.description;
                const typed: Typed<Discount> = JSON.parse(scenario.target.text);
                const output: Discount = discountTypeFactory.make(typed);

                describe(description, () => {

                    it('should produce fully populated instance', () => {
                        expect(output.total).not.toBeUndefined();
                        expect(output.validate).not.toBeUndefined();
                        expect(output.getTypeName).not.toBeUndefined();
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
const endDate: Date = new Date(new Date(2019, 2, 1));
const code: string = 'cat food';
const quantity: number = 3;
// const bulk: number = 2;
// const sale: number = 1;
// const percent: number = 0.25;
const price: number = 1.00;

class StandardDiscountSerializationScenario extends DiscountSerializationScenario {
    constructor() {
        const discount: StandardDiscount = new StandardDiscount(startDate, endDate, code, price);
        const text: string = '{"type":"StandardDiscount","thing":{"startDate":"2019-02-01T05:00:00.000Z","endDate":"2019-03-01T05:00:00.000Z","code":"cat food","price":1}}';
        super(discount, text);
    }
}

class BulkFlatPriceDiscountSerializationScenario extends DiscountSerializationScenario {
    constructor() {
        const discount: BulkFlatPriceDiscount = new BulkFlatPriceDiscount(startDate, endDate, code, quantity, price);
        const text: string = '{"type":"BulkFlatPriceDiscount","thing":{"startDate":"2019-02-01T05:00:00.000Z","endDate":"2019-03-01T05:00:00.000Z","code":"cat food","quantity":3,"price":1}}';
        super(discount, text);
    }
}