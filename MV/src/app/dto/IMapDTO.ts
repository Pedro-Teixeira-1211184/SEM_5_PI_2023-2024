export default interface IMapDTO {
  id: string;
  buildingCode: string;
  floorNumber: number;
  size: { length: number, width: number };
  map: number[][];
  rooms: {
    name: string,
    dimensions: { top: { x: number, y: number }, bottom: { x: number, y: number } },
    door: { coordinates: { x: number, y: number }, orientation: string },
  }[];
  passageways: {
    start: string,
    end: string,
    localization: {
      coordinates: { x: number, y: number },
      orientation: string
    }
  }[];
  elevator: {
    localization: {
      coordinates: { x: number, y: number },
      orientation: string
    }
  }[];
}
