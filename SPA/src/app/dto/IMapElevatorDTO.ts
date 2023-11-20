export default interface IMapElevatorDTO {
  localization: {
    coordinates: {
      x: number,
      y: number
    },
    orientation: string
  }
}
