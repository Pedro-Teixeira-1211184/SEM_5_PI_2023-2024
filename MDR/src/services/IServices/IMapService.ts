import { Result } from "../../core/logic/Result";
import IMapDTO from "../../dto/IMapDTO";
//import IPlantDTO from "../../dto/IPlantDTO";
import IPathResultDTO from "../../dto/IPathResultDTO";
import IPathDTO from "../../dto/IPathDTO";

export default interface IMapService {
  createMap(mapDTO: IMapDTO): Promise<Result<IMapDTO>>;
  //loadMap(buildingCode: string, floorNumber: number): Promise<Result<IPlantDTO>>;
  //listMaps(): Promise<Result<IPlantDTO[]>>;
  pathBetweenFloors(pathDTO: IPathDTO): Promise<IPathResultDTO | Result<IPathResultDTO>>;
  //turnToPlant(mapDTO: IMapDTO): Promise<IPlantDTO>;
  getAll(): Promise<Result<IMapDTO[]>>
}
