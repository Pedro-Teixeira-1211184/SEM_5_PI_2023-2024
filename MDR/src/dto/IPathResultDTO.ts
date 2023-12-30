import IBuildingDTO from "./IBuildingDTO";
import IPathDTO from "./IPathDTO";

export default interface IPathResultDTO {
    buildings: IBuildingDTO[];
    paths: IPathDTO[][];
  }