export default interface IMapPassagewayDTO {
  start: string;
  end: string;
  localization: {
    coordinates: {
      x: number;
      y: number;
    };
    orientation: string;
  };
}
