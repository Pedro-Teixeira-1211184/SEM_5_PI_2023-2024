import { Component,inject } from '@angular/core';
import {BuildingService} from "../../../services/building/building.service";
import IBuildingDTO from "../../../dto/IBuildingDTO";
import {FormControl, FormGroup, Validators} from "@angular/forms";

@Component({
  selector: 'app-buildings-by-floor-range',
  templateUrl: './buildings-by-floor-range.component.html',
  styleUrls: ['./buildings-by-floor-range.component.css']
})
export class BuildingsByFloorRangeComponent {
  buildingsByFloorRangeForm!: FormGroup;
  service: BuildingService = inject(BuildingService);
  allBuildings: IBuildingDTO[] = [];
  buildings: IBuildingDTO[] = [];
  showBuildings: boolean = false;
  validInput: boolean = true;

  constructor() {
    this.getAllBuildings();
  }

  ngOnInit(): void {
    this.buildingsByFloorRangeForm = new FormGroup({
      floorRange: new FormControl('', [Validators.required,Validators.minLength(3),Validators.maxLength(3)])
    });
  }

  public async getAllBuildings(){
    this.allBuildings = await this.service.getAllBuildings();
  }

  public async submit(){
    this.buildings = []; 
    this.showBuildings = true;
    if (this.buildingsByFloorRangeForm.valid) {
      this.buildings = await this.service.getBuildingsByFloorRange(this.floorRange?.value) as IBuildingDTO[];
    }
  }

  get floorRange() {
    return this.buildingsByFloorRangeForm.get('floorRange');
  }
  
}
