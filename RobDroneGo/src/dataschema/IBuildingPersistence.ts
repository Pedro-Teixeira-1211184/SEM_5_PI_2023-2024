export interface IBuildingPersistence {
    id: string;
    code: string;
    dimensions: { length: number, width: number };
    name: string;
    description: string;
    maxFloors: number;
    minFloors: number;
}
