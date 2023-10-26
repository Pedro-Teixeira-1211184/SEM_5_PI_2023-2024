import {Request, Response, NextFunction} from 'express';


export default interface IBuildingController {
  createFloor(req: Request, res: Response, next: NextFunction);
  findFloorsByBuildingCode(req: Request, res: Response, next: NextFunction);
}
