import { Result } from "../../core/logic/Result";
import IBuildingDTO from "../../dto/IBuildingDTO";

export default interface IBuildingService {
  createBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>>;
  updateBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>>;
  getBuildingByCode(buildingCode: string): Promise<Result<IBuildingDTO>>;
  getAll(): Promise<Result<Array<IBuildingDTO>>>;
  findByMinMaxFloors(min: number, max: number): Promise<Result<Array<IBuildingDTO>>>;
}
