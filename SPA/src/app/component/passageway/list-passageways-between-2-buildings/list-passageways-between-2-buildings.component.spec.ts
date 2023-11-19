import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ListPassagewaysBetween2BuildingsComponent } from './list-passageways-between-2-buildings.component';

describe('ListPassagewaysBetween2BuildingsComponent', () => {
  let component: ListPassagewaysBetween2BuildingsComponent;
  let fixture: ComponentFixture<ListPassagewaysBetween2BuildingsComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [ListPassagewaysBetween2BuildingsComponent]
    });
    fixture = TestBed.createComponent(ListPassagewaysBetween2BuildingsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
