export default interface IBuildingDTO {
    id: string;
    code: string;
    dimensions: { length: number, width: number };
    name: string;
    description: string;
    maxFloors: number;
    minFloors: number;
}
