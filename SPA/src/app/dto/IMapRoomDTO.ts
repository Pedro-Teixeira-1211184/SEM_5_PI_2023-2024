export default interface IMapRoomDTO {
    name: string;
    dimensions: {
        top: {
            x: number;
            y: number;
        };
        bottom: {
            x: number;
            y: number;
        };
    };
    door: {
        coordinates: {
            x: number;
            y: number;
        };
        orientation: string;
    };
}
