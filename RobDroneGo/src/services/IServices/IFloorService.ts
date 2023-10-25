import { Result } from "../../core/logic/Result";
import IFloorDTO from "../../dto/IFloorDTO";

export default interface IFloorService {
  createFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>>;
  findFloorsByBuildingCode(buildingCode: string): Promise<Result<IFloorDTO[]>>;
  updateFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>>;
}
