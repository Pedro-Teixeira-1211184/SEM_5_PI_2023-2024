import {Component, inject, OnInit} from '@angular/core';
import {FloorService} from "../../../services/floor/floor.service";
import {BuildingService} from "../../../services/building/building.service";
import {MapService} from "../../../services/map/map.service";
import {FormArray, FormBuilder, FormControl, FormGroup, Validators} from "@angular/forms";
import {PassagewayService} from "../../../services/passageway/passageway.service";
import IFloorDTO from "../../floor/dto/IFloorDTO";
import IBuildingDTO from "../../building/list-building/dto/IBuildingDTO";
import IPassagewayDTO from "../../passageway/dto/IPassagewayDTO";

@Component({
  selector: 'app-patch-map',
  templateUrl: './patch-map.component.html',
  styleUrls: ['./patch-map.component.css']
})
export class PatchMapComponent implements OnInit {

  service = inject(MapService);
  b_service = inject(BuildingService);
  p_service = inject(PassagewayService);
  f_service = inject(FloorService);

  mapForm!: FormGroup;
  form!: FormGroup;

  buildings: IBuildingDTO[] = [];
  floors: IFloorDTO[] = [];
  passageways: IPassagewayDTO[] = [];

  columns: number = 0;
  rows: number = 0;

  constructor(private fb: FormBuilder) {
    this.getBuildingsCodes();
  }

  ngOnInit(): void {
    this.mapForm = new FormGroup({
      buildingCode: new FormControl('', [Validators.required]),
      floorCode: new FormControl('', [Validators.required]),
    });
    // Defina a matriz e os controles do formulário ao inicializar o componente
    this.form = this.fb.group({
      matrix: this.fb.array([])
    });

    // Neste exemplo, criaremos uma matriz 3x3
    for (let i = 0; i < this.columns; i++) {
      const row = this.fb.array([]);
      for (let j = 0; j < this.rows; j++) {
        row.push(this.fb.control(0, [Validators.min(0), Validators.max(3)]));
      }
      this.matrix.push(row);
    }
  }

  initializeMatrix() {
    //reset matrix
    this.form = this.fb.group({
      matrix: this.fb.array([])
    });
    // Neste exemplo, criaremos uma matriz 3x3
    for (let i = 0; i < this.columns; i++) {
      const row = this.fb.array([]);
      for (let j = 0; j < this.rows; j++) {
        row.push(this.fb.control(0, [Validators.min(0), Validators.max(3)]));
      }
      this.matrix.push(row);
    }
  }

  private async getBuildingsCodes() {
    this.buildings = await this.b_service.getAllBuildings();
  }

  private async getFloorsCodes(buildingCode: string) {
    this.floors = [];
    this.floors = await this.f_service.getFloorsByBuildingCodeForPassageway(buildingCode);
  }

  get buildingCode() {
    return this.mapForm.get('buildingCode');
  }

  get floorCode() {
    return this.mapForm.get('floorCode');
  }

  get matrix() {
    return this.form.get('matrix') as FormArray;
  }

  public async submit() {
    //TODO: submit
    console.log(this.matrix.value);
  }

  public async getFloorsOfBuilding() {
    //set grid dimensions
    for (let i = 0; i < this.buildings.length; i++) {
      if (this.buildings[i].code == this.buildingCode?.value) {
        this.columns = this.buildings[i].dimensions.length + 1;
        this.rows = this.buildings[i].dimensions.width + 1;
        break;
      }
    }
    this.initializeMatrix();
    await this.getFloorsCodes(this.buildingCode?.value);
  }

  // Função para obter o controle de uma célula específica
  getCellControl(rowIndex: number, colIndex: number) {
    return (this.form.get('matrix') as FormGroup).get(`${rowIndex}.${colIndex}`) as FormControl;
  }

  // Função chamada quando o valor da célula é alterado
  onCellValueChange(rowIndex: number, colIndex: number) {
    const cellControl = this.getCellControl(rowIndex, colIndex);
    console.log(`Valor alterado na célula (${rowIndex}, ${colIndex}): ${cellControl?.value}`);
  }

  public async getFloorElements() {
    //get passageways
    this.passageways = await this.p_service.getPassageWayByFloorCode(this.floorCode?.value);
    //TODO: get rest of elements - rooms and elevator
  }
}
