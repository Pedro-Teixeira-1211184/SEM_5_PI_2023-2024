import {Request, Response, NextFunction} from 'express';


export default interface IPassagewayController {
  createPassageway(req: Request, res: Response, next: NextFunction);

  getPassagewaysInBuildings(req: Request, res: Response, next: NextFunction);

  updatePassageway(req: Request, res: Response, next: NextFunction);

  getPassagewaysInFloors(req: Request, res: Response, next: NextFunction);
}
