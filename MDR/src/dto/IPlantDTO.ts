export default interface IPlantDTO {
    floorCode: string;
    width: number;
    length: number;
    map: number[][];
    passageways: number[][];
    elevator: number[][];
    rooms: number[][];
}
