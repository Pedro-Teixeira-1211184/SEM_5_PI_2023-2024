import { Result } from "../../core/logic/Result";
import IPassagewayDTO from "../../dto/IPassagewayDTO";
import IFloorDTO from "../../dto/IFloorDTO";

export default interface IPassagewayService {
  createPassageway(passagewayDTO: IPassagewayDTO): Promise<Result<IPassagewayDTO>>;
  findFloorsInPassageways(floorId: string): Promise<Result<boolean>>;
  getPassagewaysInFloors(floorId: string): Promise<Result<IPassagewayDTO[]>>;
  getPassagewaysInBuildings(floors1: Array<IFloorDTO>, floors2: Array<IFloorDTO>): Promise<Result<Array<IPassagewayDTO>>>;
  updatePassageway(passagewayDTO: IPassagewayDTO): Promise<Result<IPassagewayDTO>>;
}