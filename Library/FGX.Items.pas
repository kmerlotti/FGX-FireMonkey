{*********************************************************************
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Autor: Brovin Y.D.
 * E-mail: y.brovin@gmail.com
 *
 ********************************************************************}

unit FGX.Items;

interface

uses
  System.Classes, System.Generics.Collections, FMX.Types;

type

{ TfgItemsManager }

  TfgItemInformation = record
    ItemClass: TFmxObjectClass;
    Description: string;
    constructor Create(const AItemClass: TFmxObjectClass); overload;
    constructor Create(const AItemClass: TFmxObjectClass; const ADescription: string); overload;
  end;

  /// <summary>
  ///   �������� ��� �������� ������������ ������ ���������� � ������ �������������� ������. ��������� ��������������
  ///   ����������� ������ ������ � ������������ �������� ������ ��� ����� �����������, ��������� ������� ��������
  ///   ������ FireMonkey �� ���� ����� �����������.
  /// </summary>
  /// <remarks>
  ///   ��� ��������� ������.
  /// </remarks>
  TfgItemsManager = class
  private
    class var FDictionary: TObjectDictionary<TComponentClass, TList<TfgItemInformation>>;
    class constructor Create;
    class destructor Destroy;
  public
    class procedure RegisterItem(const AComponentClass: TFmxObjectClass; const AItemInformation: TfgItemInformation);
    class procedure RegisterItems(const AComponentClass: TFmxObjectClass; const AItemsInformations: array of TfgItemInformation);
    class procedure UnregisterItem(const AComponentClass: TFmxObjectClass; const AItemInformation: TfgItemInformation);
    class procedure UnregisterItems(const AComponentClass: TFmxObjectClass; const AItemsInformations: array of TfgItemInformation);
    class function GetListByComponentClass(const AComponentClass: TFmxObjectClass): TList<TfgItemInformation>;
  end;

implementation

uses
  System.SysUtils, FGX.Asserts;

{ TfgItemInformation }

constructor TfgItemInformation.Create(const AItemClass: TFmxObjectClass);
begin
  AssertIsNotNil(AItemClass, '����� ����� ����������� ������ ���� ������');

  Self.ItemClass := AItemClass;
end;

constructor TfgItemInformation.Create(const AItemClass: TFmxObjectClass; const ADescription: string);
begin
  AssertIsNotNil(AItemClass, '����� ����� ����������� ������ ���� ������');

  Self.ItemClass := AItemClass;
  Self.Description := ADescription;
end;

{ TfgItemsManager }

class constructor TfgItemsManager.Create;
begin
  FDictionary := TObjectDictionary<TComponentClass, TList<TfgItemInformation>>.Create([doOwnsValues]);
end;

class destructor TfgItemsManager.Destroy;
begin
  FreeAndNil(FDictionary);
end;

class function TfgItemsManager.GetListByComponentClass(const AComponentClass: TFmxObjectClass): TList<TfgItemInformation>;
begin
  AssertIsNotNil(FDictionary);

  Result := nil;
  FDictionary.TryGetValue(AComponentClass, Result);
end;

class procedure TfgItemsManager.RegisterItem(const AComponentClass: TFmxObjectClass; const AItemInformation: TfgItemInformation);
var
  List: TList<TfgItemInformation>;
begin
  AssertIsNotNil(FDictionary);
  AssertIsNotNil(AComponentClass);

  if FDictionary.TryGetValue(AComponentClass, List) then
    List.Add(AItemInformation)
  else
  begin
    List := TList<TfgItemInformation>.Create;
    List.Add(AItemInformation);
    FDictionary.Add(AComponentClass, List);
  end;
end;

class procedure TfgItemsManager.RegisterItems(const AComponentClass: TFmxObjectClass; const AItemsInformations: array of TfgItemInformation);
var
  Item: TfgItemInformation;
begin
  AssertIsNotNil(FDictionary);
  AssertIsNotNil(AComponentClass);

  for Item in AItemsInformations do
    RegisterItem(AComponentClass, Item);
end;

class procedure TfgItemsManager.UnregisterItem(const AComponentClass: TFmxObjectClass; const AItemInformation: TfgItemInformation);
var
  List: TList<TfgItemInformation>;
begin
  AssertIsNotNil(FDictionary);
  AssertIsNotNil(AComponentClass);

  if FDictionary.TryGetValue(AComponentClass, List) then
  begin
    List.Remove(AItemInformation);
    if List.Count = 0 then
      FDictionary.Remove(AComponentClass);
  end;
end;

class procedure TfgItemsManager.UnregisterItems(const AComponentClass: TFmxObjectClass; const AItemsInformations: array of TfgItemInformation);
var
  Item: TfgItemInformation;
begin
  AssertIsNotNil(AComponentClass);

  for Item in AItemsInformations do
    UnregisterItem(AComponentClass, Item);
end;

end.
