import {Component, inject, OnInit} from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {MapService} from "../../services/map/map.service";
import IMapDTO from "../../dto/IMapDTO";

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.scss']
})
export class HomeComponent implements OnInit {

  service = inject(MapService);

  form!: FormGroup;

  maps: IMapDTO[] = [];
  buildings: string[] = [];
  floors: number[] = [];
  option!: IMapDTO;

  ngOnInit(): void {
    this.form = new FormGroup({
      buildingCode: new FormControl('', [Validators.required]),
      floorCode: new FormControl('', [Validators.required]),
    });
  }

  constructor() {
    this.getMaps();
  }

  private async getMaps() {
    this.maps = await this.service.getMaps();
    // different buildings
    for (let map of this.maps) {
      if (!this.buildings.includes(map.buildingCode)) {
        this.buildings.push(map.buildingCode);
      }
    }
  }

  get buildingCode() {
    return this.form.get('buildingCode');
  }

  get floorCode() {
    return this.form.get('floorCode');
  }

  submit() {
    for (let map of this.maps) {
      if (map.buildingCode == this.buildingCode?.value && map.floorNumber == this.floorCode?.value) {
        this.option = map;
        break;
      }
    }
    console.log(this.option);
    window.location.href = "/view"
  }

  getFloorsOfBuilding() {
    for (let map of this.maps) {
      if (map.buildingCode == this.buildingCode?.value) {
        this.floors.push(map.floorNumber);
      }
    }
    //order floors ascending
    this.floors.sort((a, b) => a - b);
  }
}
