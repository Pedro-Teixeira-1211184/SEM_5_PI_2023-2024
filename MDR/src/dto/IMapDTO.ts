export default class IMapDTO {
  id: string;
  buildingCode: string;
  floorNumber: number;
  size: { length: number, width: number };
  map: number[][];
  rooms: { name: string, dimensions: string, door: string }[];
  passageways: { start: string, end: string, localization: string }[];
  elevator: { localization: string };
}
