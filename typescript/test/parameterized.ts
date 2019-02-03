
export interface TestScenario<Target> {
    description: string;
    target: Target;
}

export interface ParameterizedToken<Target, Scenario extends TestScenario<Target>> {
    it(description: string, func: (value: Scenario) => void): void;
    describe(description: string, func: (value: Scenario) => void): void;
}

export class Parameterized<Target, Scenario extends TestScenario<Target>> {
    constructor(private readonly scenarios: Array<Scenario>) { }
    public forEach(predicate?: (scenario: Scenario) => boolean): ParameterizedToken<Target, Scenario> {
        return {
            it: (description: string, func: (value: Scenario) => void): void => {
                this.scenarios.forEach((scenario: Scenario): void => {
                    if (predicate === undefined || predicate(scenario)) {
                        it(description, (): void => {
                            func(scenario);
                        });
                    }
                });
            },
            describe: (description: string, func: (value: Scenario) => void): void => {
                this.scenarios.forEach((scenario: Scenario): void => {
                    if (predicate === undefined || predicate(scenario)) {
                        describe(description, (): void => {
                            func(scenario);
                        });
                    }
                });
            }
        };
    }
}
