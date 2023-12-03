import {Request, Response, NextFunction} from 'express';


export default interface IMapController {
  createMap(req: Request, res: Response, next: NextFunction);

  loadMap(req: Request, res: Response, next: NextFunction);

  listMaps(req: Request, res: Response, next: NextFunction);

  pathBetweenFloors(req: Request, res: Response, next: NextFunction);

  getAll(req: Request, res: Response, next: NextFunction);

  getAllPlants(req: Request, res: Response, next: NextFunction);
}
