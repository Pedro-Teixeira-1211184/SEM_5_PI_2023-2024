import {Request, Response, NextFunction} from 'express';


export default interface IBuildingController {
  createFloor(req: Request, res: Response, next: NextFunction);
  updateFloor(req: Request, res: Response, next: NextFunction);
  findFloorsByBuildingCode(req: Request, res: Response, next: NextFunction);
  findFloorsByPassageways(req: Request, res: Response, next: NextFunction);
}
