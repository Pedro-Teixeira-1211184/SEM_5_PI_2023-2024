export default interface IBuildingDTO {
    code: string;
    dimensions: { length: number, width: number };
    name: string;
    description: string;
    maxFloors: number;
    minFloors: number;
}
