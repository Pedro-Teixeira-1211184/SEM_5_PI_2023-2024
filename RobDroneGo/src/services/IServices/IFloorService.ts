import { Result } from "../../core/logic/Result";
import IFloorDTO from "../../dto/IFloorDTO";
import IPassagewayDTO from "../../dto/IPassagewayDTO";

export default interface IFloorService {
  createFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>>;
  findFloorsByBuildingCode(buildingCode: string): Promise<Result<IFloorDTO[]>>;
  findFloorsByPassageways(floorArray: IFloorDTO[]): Promise<Result<IFloorDTO[]>>;
  updateFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>>;
}
