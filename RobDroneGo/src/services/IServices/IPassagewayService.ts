import { Result } from "../../core/logic/Result";
import IPassagewayDTO from "../../dto/IPassagewayDTO";

export default interface IPassagewayService {
  createPassageway(passagewayDTO: IPassagewayDTO): Promise<Result<IPassagewayDTO>>;
  findFloorsInPassageways(floorId: string): Promise<Result<boolean>>;
  /*getPassagewaysInFloors(floorId: string): Promise<Result<IPassagewayDTO>>;*/
  updatePassageway(passagewayDTO: IPassagewayDTO): Promise<Result<IPassagewayDTO>>;
}