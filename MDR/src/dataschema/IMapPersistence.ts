export interface IMapPersistence {
  id: string;
  buildingCode: string;
  floorNumber: number;
  size: { height: number, width: number };
  map: number[][];
  rooms: { name: string, dimensions: string, door: string }[];
  passageways: { start: string, end: string, localization: string }[];
  elevator: { localization: string };
}
