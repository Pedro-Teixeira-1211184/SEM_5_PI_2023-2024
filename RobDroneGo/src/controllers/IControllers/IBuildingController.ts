import {Request, Response, NextFunction} from 'express';


export default interface IBuildingController {
  createBuilding(req: Request, res: Response, next: NextFunction);
  updateBuilding(req: Request, res: Response, next: NextFunction);
  getAll(req: Request, res: Response, next: NextFunction);
}
