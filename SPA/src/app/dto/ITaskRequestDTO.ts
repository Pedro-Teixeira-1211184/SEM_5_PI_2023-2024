export interface ITaskRequestDTO {
    id: { value: string };
    userEmail: string;
    startX: number;
    startY: number;
    startFloorCode: string;
    endX: number;
    endY: number;
    endFloorCode: string;
    description: string;
    taskType: string;
    robotCode: string;
    taskState: string;
}
